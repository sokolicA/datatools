#' @title DataFrame Class
#'
#' @description A class for tabular data objects of class `data.frame`.
#'
#' @import data.table
#'
#' @export
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

        sort = function(...) {
            if (!is.null(self$key)) stop("Table is already sorted with key!")
            data.table::setorder(private$.tbl, ...)
        },

        is_key_unique = function() {
            uniqueN(private$.tbl, by = self$key) == nrow(private$.tbl)
        },

        count = function(by="") {
            private$.tbl[, .N, keyby = by]
        },

        update_join = function(relationship, columns=NULL, where=NULL) {
            relationship$left <- private$.tbl
            join <- UpdateJoin$new(relationship)
            join$add_sub(columns=substitute(columns), where=substitute(where))
            return(invisible(self))
        },

        left_join = function(relationship, add=NULL) {
            relationship$left <- private$.tbl
            join <- LeftJoin$new(relationship)
            result <- join$add_sub(substitute(add))
            return(DF(result, key = self$key))
        },

        append = function(..., fill=TRUE) {
            result <- rbindlist(
                list(private$.tbl, ...),
                use.names = TRUE,
                fill = fill
            )
            return(DF(result, key = self$key))
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

        update = function(column, mapper, where=NULL) {
            if (!is.character(column)) stop("Provide column names!")
            if (!column %in% self$columns$names) stop("Nonexisting column! Use add method to add it!")
            if (!is.function(mapper)) stop("Provide a mapping function!")

            condition <- substitute(where)
            map <- parse(text=deparse(mapper))
            if (is.null(condition)) {
                private$.tbl[, (column) := eval(map)()]
            } else {
                private$.tbl[eval(condition), (column) := eval(map)()]
            }
        },

        add = function(column, mapper, where=NULL) {
            if (!(is.character(column) & length(column) == 1)) stop("Provide a single column name to add!")
            if (column %in% self$columns$names) stop("Column already exists! Use update method to update it!")
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
            removed <- DataFrame$new(private$.tbl[remove_rows], key=self$key)
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

        #' @field data Data getter.
        data = function() {
            private$.tbl
        },

        #' @field columns Column names object.
        columns = function() {
            Columns$new(private$.tbl)
        },

        #' @field key Key getter and setter.
        key = function(key) {
            if (missing(key)) return(data.table::key(private$.tbl))
            data.table::setkeyv(x=private$.tbl, cols=key)
        }
    ),

    private = list(
        .tbl = NULL,
        .id = NA_character_,

        join = NULL
    )
)


Columns <- R6::R6Class(
    "Columns",

    public = list(
        initialize = function(x) {
            private$.tbl <- x
        },

        print = function() {
            cat("Number of columns:", dim(private$.tbl)[2])
            cat("\nColumn names: ")
            cat(names(private$.tbl), sep = ", ")
        },

        #' @description Reorder columns in place
        #'
        #' @param order Character vector of the new column name ordering.
        #' May also be column numbers. If length(order) < length(x),
        #' the specified columns are moved in order to the "front" of x.
        #' By default, reorder without a specified order moves the key columns in order to the "front".
        #'
        #' @examples
        #' x <- data.table(a=1:5, b=1:5)
        #' x_cols <- Columns$new(x)
        #' x_cols$reorder(c("b", "a")) # same as x_cols$reorder("b")
        #' names(x)
        reorder = function(order=key(private$.tbl)) {
            data.table::setcolorder(private$.tbl, neworder = order)
            return(invisible(self))
        },

        #' @description Rename column names in place
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
            return(invisible(self))
        }

    ),

    active = list(
        names = function() names(private$.tbl)
    ),

    private = list(
        .tbl = NULL
    )
)
