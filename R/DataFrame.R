#' @title DataFrame Class
#'
#' @description A wrapper class for tabular data objects of class `data.frame`.
#'
#' @import data.table
DataFrame <- R6::R6Class(
    "DataFrame",
    public = list(
        #' @description `DataFrame` Constructor
        #'
        #' @param tbl An object of class `data.frame`.
        #' @param key Optional vector of column names. Setting a key sorts the table in RAM using the values of the key column(s). See Details.
        #'
        initialize = function(tbl, key=NULL) {
            data.table::setDT(tbl, key=key)
            private$tbl <- tbl
        },

        #' @description Print the table object.
        #'
        #' @details The method used is `print.data.table`.
        #'
        print = function() {
            print(private$tbl)
        },

        #' @description Return the first n rows.
        #'
        #' @param n  The number of rows to return. Defaults to 5.
        #'
        #' @return A new `DataFrame` object with only the first `n` rows.
        #' For negative values of n, this method returns all rows except the last |n| rows.
        #'
        #' @examples
        #'    df <- DF(data.frame(a=1:5, b=1:5))
        #'    df$head(1)
        head = function(n=5L) {
            DataFrame$new(head(private$tbl, n))
        },

        #' @description Return the last n rows.
        #'
        #' @param n  The number of rows to return. Defaults to 5.
        #'
        #' @return A new `DataFrame` object with only the first `n` rows.
        #' For negative values of n, this method returns all rows except the first |n| rows.
        #'
        #' @examples
        #'    df <- DF(data.frame(a=1:5, b=1:5))
        #'    df$tail(1)
        tail = function(n=5L) {
            DataFrame$new(tail(private$tbl, n))
        },

        #' @description Sort the table rows
        #'
        #' @param ...  The columns to sort by. Do not quote column names. See `?data.table::setorder`.
        #'
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #'    df <- DF(data.frame(a=1:5, b=1:5))
        #'    df$sort(-b)
        sort = function(...) {
            if (!is.null(self$key)) stop("Table is already sorted with key!")
            data.table::setorder(private$tbl, ...)
            return(invisible(self))
        },

        #' @description Check whether keys represent unique entries
        #'
        #' @return `TRUE` if the set key is unique and `FALSE` otherwise. If no key is set the result is `FALSE`.
        #'
        #' @examples
        #' df <- DF(data.frame(a=1:5, b=1:5), key = c("a"))
        #' df$is_key_unique()
        #' df <- DF(data.frame(a=1:5, b=1:5))
        #' df$is_key_unique()
        is_key_unique = function() {
            if (is.null(self$key)) return(FALSE)
            uniqueN(private$tbl, by = self$key) == nrow(private$tbl)
        },

        #' @description Count the number of rows.
        #'
        #' @param by An optional list() specifying the columns to group by. Defaults to no grouping.
        #'
        #' @return A `DataFrame` with the row counts. If `by` is specified the result will be keyed.
        #'
        #' @examples
        #' df <- DF(data.frame(a=1:5, b=1:5))
        #' df$count()
        #' df <- DF(data.frame(a=c(1,1,1,2,3), b=1:5))
        #' df$count(by = list(a)) # df$count(by = .(a))
        count = function(by=NULL) {
            result <- eval(substitute(private$tbl[, .N, keyby = by]))
            DataFrame$new(result)
        },

        #' @description Create a new `DataFrame` with filter applied to the rows.
        #'
        #' @param keep An expression to be evaluated inside the table, integer vector specifying rows to remove or a logical vector. See details
        #'
        #' @details
        #' \itemize{
        #' \item If an expression is passed it will be evaluated inside the context of the table.
        #' \item If an integer vector is passed, the rows specified will be kept. Passing duplicated numbers will result in duplicated rows and passing numbers larger than the number of rows will result in `NA` rows.
        #' \item If a logical vector is passed it must be of the same length as the number of rows. Logical `NA` values are treated as `FALSE` and those rows will not be removed.
        #'}
        #'
        #' @return A new `DataFrame` with kept rows.
        #' @examples
        #' df <- DF(data.frame(a=1:5, b=1:5))
        #' df$filter(a > 2)
        #' df$filter(c(1, 3, 5))
        #' df$filter(c(TRUE, NA, FALSE, FALSE, TRUE))
        filter = function(keep) {
            DataFrame$new(eval(substitute(private$tbl[keep])))
        },

        #' @description Filter the table in place.
        #'
        #' Same as `$filter` method, just done in place. Opposite of the `$remove` method.
        #'
        #' @param keep An expression to be evaluated inside the table, integer vector specifying rows to remove or a logical vector. See details
        #'
        #' @details
        #' \itemize{
        #' \item If an expression is passed it will be evaluated inside the context of the table.
        #' \item If an integer vector is passed, the rows specified will be kept. Passing duplicated numbers will result in duplicated rows and passing numbers larger than the number of rows will result in `NA` rows.
        #' \item If a logical vector is passed it must be of the same length as the number of rows. Logical `NA` values are treated as `FALSE` and those rows will not be removed.
        #'}
        #'
        #' @return Returns itself with kept rows only.
        #' @examples
        #' df <- DF(data.frame(a=1:5, b=1:5))
        #' df$filter_(a > 2)
        #' df
        filter_ = function(keep) {
            private$tbl <- eval(substitute(private$tbl[keep]))
            self
        },

        #' @description Remove specified rows from the table in place.
        #'
        #' @param where An expression to be evaluated inside the table, integer vector specifying rows to remove or a logical vector. See details
        #'
        #' @details
        #' \itemize{
        #' \item If an expression is passed it will be evaluated inside the context of the table.
        #' \item If an integer vector is passed, the rows specified will be removed. Passing duplicated numbers or numbers larger than the number of rows will result in an error.
        #' \item If a logical vector is passed it must be of the same length as the number of rows. Logical `NA` values are treated as `FALSE` and those rows will not be removed.
        #'}
        #'
        #' @return A `DataFrame` object consisting of removed rows.
        #' @examples
        #' df <- DF(data.frame(a=1:5, b=1:5))
        #' df$remove(a > 2)
        #' df <- DF(data.frame(a=1:5, b=1:5))
        #' df$remove(c(1, 3, 5))
        #' df <- DF(data.frame(a=1:3, b=1:3))
        #' df$remove(c(TRUE, NA, FALSE))
        remove = function(where) {
            condition <- substitute(where)
            if (!is.language(condition)) {
                stop(private$err$remove$not_language)
            }

            remove_rows <- eval(condition, envir=private$tbl, enclos=parent.frame())
            if (is.numeric(remove_rows)) {
                if (max(remove_rows) > private$tbl[,.N]) stop(private$err$remove$int_out_of_bounds)
                if (any(duplicated(remove_rows))) stop(private$err$remove$int_duplicated)
                remove_rows <- private$tbl[, .I %in% remove_rows]
                keep <- !remove_rows
            }
            if (is.logical(remove_rows)) {
                if(length(remove_rows) != private$tbl[,.N]) stop(private$err$remove$unequal_length_logical)
                keep <- !remove_rows | is.na(remove_rows)
            }

            removed <- DataFrame$new(private$tbl[remove_rows], key=self$key)
            private$tbl <- private$tbl[keep]
            return(removed)
        },

        #' @description Create a key the table.
        #'
        #' `set_key` sorts the table and marks it as sorted with an attribute sorted.
        #'
        #' @param key A Character vector of column names to set the key on.
        #'
        #' @return Invisibly returns itself.
        set_key = function(key) {
            data.table::setkeyv(x=private$tbl, cols=key)
            invisible(self)
        },

        #' @description Create a new `DataFrame` by appending tables using column names.
        #'
        #' @param ... Objects of class `data.frame` or `DataFrame`.
        #' @param fill Optional parameter whether to fill missing columns with `NA`. Defaults to `FALSE`.
        #'
        #' @return A new unkeyed `DataFrame` object with rows appended.
        #'
        #' @examples
        #' x <- data.frame(a=1:5, b=1:5)
        #' y <- data.frame(a=1:5, b=1:5)
        #' df <- DF(x)
        #' res <- df$append(y, x, y, df)
        append = function(..., fill=FALSE) {
            append <- list(private$tbl, ...)
            tbls <- lapply(append, function(x) {
                if(inherits(x, "DataFrame")) return(x$unwrap())
                return(x)
            })

            result <- rbindlist(
                tbls,
                use.names = TRUE,
                fill = fill
            )
            return(DF(result, key=NULL))
        },

        #' @description Append tables to the `DataFrame`.
        #' Same as the `$append` method but without creating a new object.
        #'
        #' @param ... Objects of class `data.frame` or `DataFrame`.
        #' @param fill Optional parameter whether to fill missing columns with `NA`. Defaults to `FALSE`.
        #'
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #' x <- data.frame(a=1:5, b=1:5)
        #' y <- data.frame(a=1:5, b=1:5)
        #' df <- DF(x)
        #' df$append_(y, x, y, df)
        append_ = function(..., fill=FALSE) {
            append <- list(private$tbl, ...)
            tbls <- lapply(append, function(x) {
                if(inherits(x, "DataFrame")) return(x$unwrap())
                return(x)
            })

            private$tbl <- rbindlist(
                tbls,
                use.names = TRUE,
                fill = fill
            )
            invisible(self)
        },

        #' @description Get the underlying data.
        #'
        #' @return The underlying `data.table` object.
        #'
        unwrap = function() {
            private$tbl
        }
    ),

    active = list(

        #' @field columns Column names object.
        columns = function() {
            Columns$new(self)
        },

        #' @field key Key getter.
        key = function() {
            data.table::key(private$tbl)
        }
    ),

    private = list(

        tbl = NULL,

        deep_clone = function(name, value) {
            if (name == "tbl") return(data.table::copy(value))
            value
        }
    )
)
