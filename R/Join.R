Join <- R6::R6Class(
    "Join",
    public = list(
        initialize = function(relationship) {
            # TODO assert that data.table. or DataFrame?
            if (relationship$is_not_fully_specified()) stop("Relationship has missing specification!")
            private$rel <- relationship
        },

        execute = function(cmd) {
            stop("Method must be used in subclass!")
        }
    ),
    private = list(
        rel = NULL,
        columns = NULL,
        column_names = NULL,

        add_missing_expr_names = function(e) {
            result <- if (!is.null(names(e))) names(e) else vector("character", length=length(e))
            missing <- which(result == "")[-1L]
            if (any(missing)) {
                for (i in missing) {
                    val <- deparse(e[[i]])
                    result[i] <- if(grepl(">|<", val)) "" else val
                }
                names(e) <- result
            }
            e
        },

        convert_langlist_to_vector = function(langlist) {
            langlist <- langlist[-1L]
            res <- vector(length = length(langlist))
            for (i in seq_along(res)) {
                res[i] <- deparse(langlist[[i]])
            }
            names(res) <- if (!is.null(names(langlist))) names(langlist) else res
            res
        },

        convert_vector_to_langlist = function(vec){
            str2lang(paste0("list(", paste0(names(vec), "=", vec, collapse = ", "), ")"))
        }
    )
)

UpdateJoin <- R6::R6Class(
    "UpdateJoin",
    inherit = Join,
    public = list(
        #https://stackoverflow.com/questions/54312225/which-data-table-syntax-for-left-join-one-column-to-prefer
        execute = function(col_expr=NULL, where_expr=NULL) {
            private$where <- where_expr
            if (is.null(col_expr)) {
              private$set_columns_add_all()
            } else {
                private$columns <- private$add_missing_expr_names(col_expr)
                private$column_names <- names(private$columns)[-1]
            }
            private$execute_call()
        }
    ),

    private = list(

        where = NULL,

        execute_call = function() {
            tryCatch(
                eval(private$build_call()),
                error = function(e) {
                    if (grepl(private$one_many_join_err, e$message, fixed=TRUE)) {
                        stop("Can not perform update join with a one-to-many relationship!")
                    }
                    stop(e$message)
                })
            private$rel$left[]
        },

        build_call = function() {
            ON <- private$rel$reverse()$on
            NAMES <- private$column_names
            J <- private$columns
            e <- substitute(private$rel$left[, `:=`(NAMES, private$rel$right[i = .SD, on = ON, j = J, mult = "all", nomatch = NA])])
            if (!is.null(private$where)) e[[3]] <- private$where
            e
        },

        set_columns_add_all = function() {
            columns <- setdiff(names(private$rel$right), private$rel$right_on())
            if (length(columns) == 0) return(NULL)
            cols_x <- paste0("x.", columns)
            private$columns <- str2lang(paste0("list(", paste(cols_x, collapse = ", "), ")"))
            private$column_names <- ifelse(columns %in% names(private$rel$left), paste0(columns, "_y"), columns)
        },

        one_many_join_err = "If you wish to 'recycle' the RHS please use rep() to make this intent clear to readers of your code."
    )
)

LeftJoin <- R6::R6Class(
    "LeftJoin",
    inherit = Join,
    public = list(
        execute = function(col_expr=NULL) {
            if (is.null(col_expr)) {
                return(private$full_join())
            } else {
                private$columns <- private$add_missing_expr_names(col_expr)
                return(private$specific_join())
            }
        }
    ),

    private = list(

        full_join = function() {
            result <- eval(private$build_call())
            private$fix_names(result);
            data.table::setcolorder(result, names(private$rel$left))
            result[]
        },

        specific_join = function() {
            result <- eval(private$build_call())
            private$prefix_all_added_columns(result)
            private$update_columns(result)
            private$reset_names(result)
            data.table::setcolorder(result, names(private$rel$left))
            result[]
        },

        build_call = function() {
            ON <- private$rel$reverse()$on
            substitute(private$rel$right[private$rel$left, on = ON, mult = "all", nomatch = NA])
        },

        update_columns = function(x) {
            J <- private$columns
            J[[1]] <- substitute(`:=`)
            to_remove <- setdiff(setdiff(names(private$rel$right), private$rel$right_on()), names(J))
            for (col in to_remove) J[col] <- list(NULL)
            eval(substitute(x[, j = J]), parent.frame())
        },

        fix_names = function(result) {
            keys_left <- private$rel$reverse()$right_on()
            keys_right <- private$rel$right_on()
            duplicated_names <- intersect(names(private$rel$left), names(private$rel$right))
            new_left <- setdiff(duplicated_names, keys_left)
            names_r <- setdiff(duplicated_names, keys_right)
            names_l <- paste0("i.", new_left)
            new_right <- paste0(names_r, "_y")
            data.table::setnames(
                result,
                c(keys_right, names_r, names_l),
                c(keys_left, new_right, new_left)
            )
        },

        prefix_all_added_columns = function(x) {
            #TODO The idx logic below could probably be simplified
            keys_left <- private$rel$reverse()$right_on()
            keys_right <- private$rel$right_on()
            idx_keys_x <- names(private$rel$left) %in% keys_left
            idx_duplicated <- names(private$rel$left) %in% names(private$rel$right) & !idx_keys_x
            idx <- !(idx_keys_x | idx_duplicated)

            data.table::setnames(
                x,
                c(keys_right, names(private$rel$left)[idx]),
                paste0("i.", c(keys_left, names(private$rel$left)[idx]))
            )
        },

        reset_names = function(x) {
            duplicated_names <- intersect(names(private$rel$left), names(private$columns))
            setnames(x,
                     c(paste0("i.", names(private$rel$left)), duplicated_names),
                     c(names(private$rel$left), paste0(duplicated_names, "_y")))
        }

    )
)
