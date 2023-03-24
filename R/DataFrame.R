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

        extract = function(where) {

        },

        deep_clone = function() {
            result <- self$clone(deep=TRUE)
            result_private <- .subset2(result, ".__enclos_env__")$private
            result_private$.tbl <- copy(private$.tbl)
            return(result)
        }

    ),

    private = list(
        .tbl = NULL,
        .id = NA_character_
    )
)
