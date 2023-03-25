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

        #' Rename column names in place
        #'
        #' @param mapper Function
        #'
        #' @examples
        #' x <- data.table(a=1:5, b=1:5)
        #' df <- DataFrame$new(x)
        #' df$rename(toupper)
        #' custom_mapper = function(x) {return(paste0(x, 1))}
        #' df$rename(custom_mapper)
        rename = function(mapper) {
            if (!is.function(mapper)) stop("Provide a function that maps old names to new names!")
            data.table::setnames(private$.tbl, old=mapper)
        },

        key = function() {
            data.table::key(private$.tbl)
        },

        set_key = function(key) {
            data.table::setkeyv(x=private$.tbl, cols=key)
        },

        is_key_unique = function() {
            uniqueN(private$.tbl, by = self$key()) == nrow(private$.tbl)
        },

        count = function(by="") {
            private$.tbl[, .N, keyby = by]
        },

        append = function(rows) {

        },

        apply = function(columns, mapper, where=NULL) {
            if (!is.character(columns)) stop("Provide column names!")
            if (!is.function(mapper)) stop("Provide a mapping function!")

            condition <- substitute(where)
            map <- parse(text=deparse(mapper))
            if (!is.null(condition)) {
                private$.tbl[eval(condition), (c(columns)) := lapply(.SD, eval(map)), .SDcols = columns]
            } else {
                private$.tbl[, (c(columns)) := lapply(.SD, eval(map)), .SDcols = columns]
            }
        },

        update = function(columns, mapper, where=NULL) {
            if (!is.character(columns)) stop("Provide column names!")
            if (!is.function(mapper)) stop("Provide a mapping function!")

            condition <- substitute(where)
            map <- parse(text=deparse(mapper))
            if (!is.null(condition)) {
                private$.tbl[eval(condition), (c(columns)) := lapply(.SD, eval(map)), .SDcols = columns]
            } else {
                private$.tbl[, (c(columns)) := lapply(.SD, eval(map)), .SDcols = columns]
            }
        },

        add = function(column, mapper, where=NULL) {
            if (!(is.character(column) & length(column) == 1)) stop("Provide a single column name to add!")
            if (column %in% self$columns) stop("Column already exists! Use update method to update it!")
            if (!is.function(mapper)) stop("Provide a mapping function!")

            condition <- substitute(where)
            map <- parse(text=deparse(mapper))
            if (is.null(condition)) {
                private$.tbl[, (column) := eval(map)()]
            } else {
                private$.tbl[eval(condition), (column) := eval(map)()]
            }
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
