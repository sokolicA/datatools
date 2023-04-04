#' Join Class
#'
#' @param relationship
#'
#' @return
#'
#' @import data.table
Join <- R6::R6Class(
    "Join",
    public = list(
        initialize = function(relationship) {
            # TODO assert that data.table. or DataFrame?
            if (relationship$is_not_fully_specified()) stop("Relationship has missing specification!")
            private$left <- relationship$left
            private$right <- relationship$right
            private$.on <- relationship$get_on()
        },

        test = function() {
            browser()
        },

        print = function() {

        },

        add = function(...) {
            stop("Method must be used in subclass!")
        },

        add_all = function() {
            stop("Method must be used in subclass!")
        }
    ),
    private = list(
        left = NULL,
        right = NULL,
        .on = NULL,
        .add = NULL,

        command = function() {
            stop("Method must be used in subclass!")
        },

        execute = function(cmd) {
            eval(parse(text = cmd), envir = parent.frame())
        },

        swap_on_sub = function() {
            result <- private$.on
            names <- names(result)
            new_names <- names
            for (i in seq_along(result)[-1L]) {
                new_names[i] <- deparse(result[[i]])
                result[[i]] <- str2lang(names[i])
            }
            names(result) <- new_names
            result
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
        },

        swap_names_values = function(vec) {
            if (!is.character(vec)) stop("Must provide a character vector!")
            result <- names(vec)
            names(result) <- vec
            return(result)
        }
    )
)

UpdateJoin <- R6::R6Class(
    "UpdateJoin",
    inherit = Join,
    public = list(
        add_sub = function(columns, where=NULL) {
            private$.where <- where
            if (is.null(columns)) return(self$add_all_sub())
            private$.add <- parse_dots(columns)
            private$.update_join()
        },

        add_all_sub = function() {
            private$build_add_all_sub()
            private$.update_join()
        },

        add = function(columns, where=NULL) {
            private$.add <- parse_dots(substitute(columns))
            private$.where <- substitute(where)
            private$.update_join()
        },

        add_all = function(where=NULL) {
            private$.where <- substitute(where)
            private$build_add_all_sub()

            private$.update_join()
        }
    ),

    private = list(

        .where = NULL,

        .update_join = function() {
            tryCatch(
                eval(private$build_call()),
                error = function(e) {
                    if (grepl(private$one_many_join_err, e$message, fixed=TRUE)) {
                        stop("Can not perform update join with a one-to-many relationship!")
                    }
                    stop(e$message)
                })
            private$left[]
        },

        build_call = function() {
            call_sub_j_inner <- substitute(`[` (private$right, i=.SD, on=.(), j=.(), mult="all", nomatch=NA))
            call_sub_j_inner[[4]] <- private$swap_on_sub()
            call_sub_j_inner[[5]] <- private$.add #names(private$.add) <- NULL
            call_sub_j <- substitute(`:=`(c(), call_sub_j_inner))
            call_sub_j[[2]] <- names(private$.add)[-1]
            call_sub_j[[3]] <- call_sub_j_inner
            call <- substitute(`[` (private$left, i, j=call_sub_j))
            call[[3]] <- if (!is.null(private$.where)) private$.where else substitute(,)
            call[[4]] <- call_sub_j
            call
        },

        build_add_all_sub = function() {
            key_pairs <- private$convert_langlist_to_vector(private$.on)
            names_r <- setdiff(names(private$right), key_pairs)
            names_l <- names(private$left)
            add_r <- paste0("x.", names_r)
            new_right <- ifelse(names_r %in% names_l, paste0(names_r, "_y"), names_r)
            private$.add <- str2lang(paste0("list(", paste0(
                paste0(new_right, "=", names_r), collapse = ", "), ")"))
        },

        one_many_join_err = "If you wish to 'recycle' the RHS please use rep() to make this intent clear to readers of your code."
    )
)

LeftJoin <- R6::R6Class(
    "LeftJoin",
    inherit = Join,
    public = list(
        add_sub = function(columns) {
            if (is.null(columns)) return(private$full_join())
            private$.add <- parse_dots(columns)
            private$specific_join()
        },

        add = function(columns=NULL) {
            columns_sub <- substitute(columns)
            self$add_sub(columns_sub)
        },

        add_all = function() {
            private$full_join()
        }

    ),

    private = list(

        build_call_full = function() {
            call <- substitute(`[` (private$right, i=private$left, on = .(),
                                    mult="all", nomatch=NA))
            call[[4]] <- private$swap_on_sub()
            call
        },

        build_call_specific = function() {
            key_pairs <- private$convert_langlist_to_vector(private$.on)

            call <- quote(`[`(result, , j = j_sub))
            add <- private$.add
            add[[1]] <- substitute(`:=`)
            to_remove <- setdiff(setdiff(names(private$right), key_pairs), names(add))
            for (col in to_remove) add[col] <- list(NULL)
            call[[4]] <- add
            call
        },

        full_join = function() {
            result <- eval(private$build_call_full())
            private$.fix_names(result);
            data.table::setcolorder(result, names(private$left))
            result[]
        },

        specific_join = function() {
            result <- eval(private$build_call_full())
            key_pairs <- private$convert_langlist_to_vector(private$.on)

            private$.prefix_all_added_columns(result, key_pairs)

            result <- eval(private$build_call_specific())

            private$reset_names(result)

            data.table::setcolorder(result, names(private$left))
            result[]
        },

        .fix_names = function(result) {
            key_pairs <- private$convert_langlist_to_vector(private$.on)
            duplicated_names <- intersect(names(private$left), names(private$right))
            new_left <- setdiff(duplicated_names, names(key_pairs))
            names_r <- setdiff(duplicated_names, key_pairs)
            names_l <- paste0("i.", new_left)
            new_right <- paste0(names_r, "_y")
            data.table::setnames(
                result,
                c(key_pairs, names_r, names_l),
                c(names(key_pairs), new_right, new_left)
            )
        },

        .prefix_all_added_columns = function(x, key_pairs) {
            #TODO The idx logic below could probably be simplified
            idx_keys_x <- names(private$left) %in% names(key_pairs)
            idx_duplicated <- names(private$left) %in% names(private$right) & !idx_keys_x
            idx <- !(idx_keys_x | idx_duplicated)

            data.table::setnames(
                x,
                c(key_pairs, names(private$left)[idx]),
                paste0("i.", c(names(key_pairs), names(private$left)[idx]))
            )
        },

        reset_names = function(x) {
            duplicated_names <- intersect(names(private$left), names(private$.add))
            setnames(x,
                     c(paste0("i.", names(private$left)), duplicated_names),
                     c(names(private$left), paste0(duplicated_names, "_y")))
        }

    )
)
