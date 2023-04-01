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
        add = function(where=NULL, ...) {
            private$.add <- parse_dots(substitute(list(...)))
            private$.update_join(substitute(where))
        },

        add_all = function() {
            private$.update_join()
        }
    ),

    private = list(
        .update_join = function(where) {
            key_pairs_orig <- private$convert_langlist_to_vector(private$.on)
            key_pairs <- private$swap_names_values(key_pairs_orig)

            cmd <- private$cmd_update(where, deparse(private$convert_vector_to_langlist(key_pairs)))
            private$execute(cmd)
            private$left[]
        },

        cmd_update = function(where, on_string) {
            names <- names(private$.add)[-1]
            paste0("private$left[", deparse(where),
                   ", (names(private$.add)[-1]) := private$right[.SD, on =", on_string,
                   ", ", deparse(private$.add), ", mult = 'all', nomatch = NA]]")
        }
    )
)

LeftJoin <- R6::R6Class(
    "LeftJoin",
    inherit = Join,
    public = list(
        add = function(...) {
            private$.add <- parse_dots(substitute(list(...)))
            private$.left_join()
        },

        add_all = function() {
            private$.left_join()
        }

    ),

    private = list(
        .left_join = function() {
            key_pairs <- private$convert_langlist_to_vector(private$.on)
            cmd <- private$cmd_left_join(key_pairs)
            result <- private$execute(cmd)

            private$.prefix_all_added_columns(result, key_pairs)

            if (!is.null(private$.add)) private$.modify_cols(result, key_pairs)

            private$.fix_names(result)
            data.table::setcolorder(result, names(private$left))

            result[]
        },

        .modify_cols = function(result, key_pairs) {
            add <- gsub("^list", "", deparse(private$.add))
            private$execute(paste("result[, `:=`", add, "]"))
            not_used <- names(private$right)[!names(private$right) %in% c(names(private$.add), key_pairs)]
            if (length(not_used) > 0) result[, (not_used) := NULL]
            result
        },

        .fix_names = function(result) {
            old <- c(names(private$right), paste0("i.", names(private$left)))
            new <- c(paste0(names(private$right), "_y"), names(private$left))
            data.table::setnames(result, old, new, skip_absent = TRUE) #skipping key of y
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

        cmd_left_join = function(key_pairs) {
            swapped_key_pairs <- private$swap_names_values(key_pairs)
            on_string <- deparse(private$convert_vector_to_langlist(swapped_key_pairs))
            paste0(
                "private$right[private$left, on =", on_string,
                ", mult = 'all', nomatch = NA]"
            )
        }

    )
)
