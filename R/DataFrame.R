#' Title
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
DataFrame <- R6::R6Class(
    "DataFrame",
    public = list(
        initialize = function(tbl, key = NULL, id = NULL) {
            data.table::setDT(tbl, key = key)
            private$.tbl <- tbl
            private$.id <- id
        },

        print = function() {
          print(private$.tbl)
        },

        data = function() {
            private$.tbl
        },

        key = function() {
            data.table::key(private$.tbl)
        },

        set_key = function(key) {
            data.table::setkeyv(x=private$.tbl, cols=key)
        },

        is_key_unique = function() {
            uniqueN(private$.tbl, by = get_key(private$.tbl)) == nrow(private$.tbl)
        },

        count = function(by="") {
            private$.tbl[, .N, keyby = by]
        },

        append = function(rows) {

        },

        drop = function(columns) {
            if (!is.character(columns)) stop("Provide a vector of column names!")
            private$.tbl[, (c(columns)) := NULL]
        },

        remove = function(where) {
            condition <- substitute(where)
            if (!is.language(condition)) {
                stop("Provide either an expression, an integer vector specifying rows or a logical vector")
            }
            remove_rows <- eval(substitute(where), envir=private$.tbl, enclos=parent.frame())
            if (is.logical(remove_rows) & length(remove_rows) != private$.tbl[,.N]) stop("Condition length does not match data size!")
            if (is.numeric(remove_rows)) {
                if (max(remove_rows) > private$.tbl[,.N]) stop("Rows specified are out of bounds!")
                if (any(duplicated(remove_rows))) stop("Duplicated row numbers not allowed!")
            }
            removed <- DataFrame$new(private$.tbl[remove_rows], key=self$key())
            private$.tbl <- private$.tbl[!remove_rows]
            return(removed)
        },

        deep_clone = function() {
            result <- self$clone(deep=TRUE)
            result_private <- .subset2(result, ".__enclos_env__")$private
            result_private$.tbl <- copy(private$.tbl)
            return(result)
        }

    ),

    active = list(
      columns = function() {
          names(private$.tbl)
      }
    ),

    private = list(
        .tbl = NULL,
        .id = NA_character_
    )
)
