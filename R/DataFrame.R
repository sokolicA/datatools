#' @title DataFrame Class
#'
#' @description A wrapper class for tabular data objects of class `data.frame`.
#'
#' @import data.table
#' @import R6
#' @include RcppExports.R
DataFrame <- R6::R6Class(
    "DataFrame",
    public = list(
        #' @description `DataFrame` Constructor
        #'
        #' @param tbl An object of class `data.frame`.
        #' @param key Optional vector of column names. Setting a key sorts the table in RAM using the values of the key column(s). See Details.
        #'
        initialize = function(tbl, key=NULL) {
            data.table::setDT(tbl)
            private$tbl <- tbl
            if (!is.null(key)) self$set_key(key)
        },

        #' @description Print the table object.
        #'
        #' @details The method used is `print.data.table`.
        #'
        print = function() {
            if (self$is_grouped()) cat("Grouped by:", gsub("(^list\\()|(\\)$)", "", deparse1(private$grp_expr)), "\n")
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
        #' @return A `DataFrame` with the row counts. The result will be keyed by the grouping if a grouped `DataFrame` was used.
        #'
        #' @examples
        #' df <- DF(data.frame(x=1:5, g = c("a", "a", "b", "c", "c")))
        #' df$count()
        #' df$group_by(g)$count()
        count = function() {
            by <- private$grp_expr
            expr <- substitute(private$tbl[, .N, keyby = by])
            DataFrame$new(eval(expr))
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

        #' @description Group or un-group the data.
        #' Used in calculation of statistics.
        #'
        #' @param ... An expression specifying by what to group the data. See details.
        #'
        #' @details Setting by Will override existing grouping without warning.
        #' Pass a character vector of groups `vec` using `c(vec)`.
        #' Set to `NULL` to remove any existing grouping.
        #'
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #' df <- DF(data.table(a=1:5, b=3))
        #' df$group(a)
        #' df$group("a")
        #' df$group(a, b)
        #' df$group(c("a", "b"))
        #' df$group(a > 2)
        #' df$group(s = a > 2) # will name the grouping column with s
        #' a <- "b"
        #' df$group(a) #will group by a
        #' df$group(c(a)) # will group by b
        #' df$group(NULL) # will remove grouping
        group_by = function(...) {
            e <- substitute(alist(...))[-1L]
            result <- private$parse_group_expr(e)
            check <- try(eval(substitute(private$tbl[0][, .N, by = result])), silent=TRUE)
            if (inherits(check, "try-error")) stop(attr(check, "condition")$message)
            private$grp_expr <- result
            invisible(self)
        },

        #' @description Check whether the data is grouped.
        #'
        #' @return TRUE or FALSE.
        #'
        #' @examples
        #' df <- DF(data.table(a=1:5, b=3))
        #' df$is_grouped()
        #' df$group(a)$is_grouped()
        is_grouped = function() {
            !is.null(private$grp_expr)
        },

        #' @description Remove specified rows from the table in place.
        #'
        #' @param where An expression to be evaluated inside the table, integer vector specifying rows to remove or a logical vector. See details
        #'
        #' @details
        #' \itemize{
        #' \item If an expression is passed it will be evaluated inside the context of the table, treating columns as variables.
        #' \item If an integer vector is passed, the rows specified will be removed. Passing duplicated numbers will return duplicated rows as removed. `NA` values are treated as `FALSE` and those rows will not be removed.
        #' \item If a logical vector is passed it must be of the same length as the number of rows. `NA` values are treated as `FALSE` and those rows will not be removed.
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
            if (try(is.logical(where), silent=TRUE) == TRUE && length(where) != dim(private$tbl)[1]) {
                stop("Logical vectors must be of equal length as the number of table rows.")
            }
            idx_remove <- eval(substitute(private$tbl[, .I[where]]))
            idx_remove <- private$rm_na_int(idx_remove)
            removed <- DataFrame$new(private$tbl[idx_remove])
            private$tbl <- private$tbl[!idx_remove]
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
        #'
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
            Columns$new(private)
        },

        #' @field key Key getter.
        key = function() {
            data.table::key(private$tbl)
        }
    ),

    private = list(

        tbl = NULL,

        grp_expr = NULL,
        grp_cols = NULL,

        deep_clone = function(name, value) {
            if (name == "tbl") return(data.table::copy(value))
            value
        },

        parse_group_expr = function(e) {
            result <- quote(list())
            idx <- 2L
            for(i in seq_along(e)) {
                el <- e[[i]]
                if (is.name(el)) {
                    result[[idx]] <- el
                    if (!is.null(names(e))) names(result)[[idx]] <- names(e)[i]
                    idx <- idx + 1L
                    next
                }
                if (is.null(el)) {
                    result <- NULL
                    break
                }
                if (is.character(el)) {
                    result[[idx]] <- as.name(el)
                    idx <- idx + 1L
                    next
                }
                if (!is.name(el) && el[[1]] == quote(c)) {
                    N <- 2L
                    while(TRUE) {
                        cols <- try(eval(el, parent.frame(n=N)), silent=TRUE)
                        if (!inherits(cols, "try-error")) break
                        if (identical(globalenv(), parent.frame(n=N))) {
                            stop(attr(cols, "condition")$message)
                        }
                        N <- N+1L
                    }
                    for (col in cols) {
                        result[[idx]] <- as.name(col)
                        idx <- idx + 1L
                    }
                    next
                }
                if (is.call(el) &&  grepl("!|>|<|=|%",  as.character(el[[1]]))) {
                    result[[idx]] <- el
                    if (!is.null(names(e))) names(result)[[idx]] <- names(e)[i]
                    idx <- idx + 1L
                    next
                }
                if (is.call(el)) {
                    result[[idx]] <- el
                    if (!is.null(names(e))) names(result)[[idx]] <- names(e)[i]
                    idx <- idx + 1L
                    next
                }
                stop("Do not know how to interpret given grouping: ", as.character(el), ".")
            }
            return(result)
        },

        rm_na_int = remove_na_integer
    )
)
