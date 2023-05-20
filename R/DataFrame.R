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
            d <- dim(private$tbl)
            cat("Wrapping a", d[1], "x", d[2], "data.table.\n")
            if (self$is_grouped()) cat("Grouped by:", gsub("(^list\\()|(\\)$)", "", deparse1(private$keyby)), "\n")
            if (!is.null(private$i)) cat("Using rows where:", private$i_txt, "\n")
            if (!is.null(private$sdcols)) cat("Columns subset using:", private$sdcols_txt, "\n")
            print(private$tbl_subset())
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
            result <- private$tbl_subset()
            DataFrame$new(head(result, n))
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
            result <- private$tbl_subset()
            DataFrame$new(tail(result, n))
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
            result <- private$tbl_eval(i=private$i,j=quote(list(.N)), keyby=private$keyby)
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

        #' @description Group or un-group the data.
        #' Used in calculation of statistics.
        #'
        #' @param ... An expression specifying by what to group the data. See details.
        #' @param persist Optional parameter whether the grouping should persist after the first evaluation. Defaults to FALSE.
        #'
        #' @details Setting by Will override existing grouping without warning.
        #' Pass a character vector of groups `vec` using `c(vec)`.
        #' Set to `NULL` to remove any existing grouping.
        #'
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #' df <- DF(data.table(a=1:5, b=3))
        #' df$group_by(a)
        #' df$group_by("a")
        #' df$group_by(a, b)
        #' df$group_by(c("a", "b"))
        #' df$group_by(a > 2)
        #' df$group_by(s = a > 2) # will name the grouping column with s
        #' a <- "b"
        #' df$group_by(a) #will group by a
        #' df$group_by(c(a)) # will group by b
        #' df$group_by(NULL) # will remove grouping
        group_by = function(..., persist=FALSE) {
            if (!is_true_or_false(persist)) stop("Persist must be either true (1) or false (0).")
            e <- substitute(alist(...))[-1L]
            result <- private$parse_keyby(e)
            check <- try(eval(substitute(private$tbl[0][, .N, by = result])), silent=TRUE)
            if (inherits(check, "try-error")) stop(attr(check, "condition")$message)
            private$keyby <- result
            private$keyby_persist <- persist
            invisible(self)
        },

        #' @description Work (operate?) on a subset of rows.
        #' Experimental. #TODO define how certain methods use the persist option
        #' This method will not remove the rows from the data.
        #' Selected data modifications or calculations will be based only on the selected subset of data.
        #'
        #'
        #' @param rows An expression to be evaluated inside the table, integer vector specifying rows to remove or a logical vector.
        #' @param persist Optional parameter whether the subset should persist after evaluation.
        #'
        #' @details
        #' If `rows` is missing or `NULL` then all rows will be used.
        #'
        #' @return Invisibly returns itself.
        where = function(rows, persist=FALSE) {
            if (missing(rows)) rows <- NULL
            if (!is_true_or_false(persist)) stop("Persist must be either true (1) or false (0).")
            private$i <- private$parse_i(substitute(rows), parent.frame())
            private$i_txt <- deparse1(substitute(rows))
            private$i_persist <- persist
            private$i_env = parent.frame()
            invisible(self)
        },

        #' @description Work on a subset of the columns.
        #' Experimental. #TODO define behaviour of other methods.
        #' This method will not remove the columns from the data.
        #' Selected data modifications or calculations will be based only on the selected subset of data.
        #'
        #' $select(mean(x) >5) --> df[, .SD, .SDcols = sapply(df, function(x) mean(x) > 5)]
        #' $select(mean(is.na(x)) >0.2) --> df[, .SD, .SDcols = sapply(df, function(x) mean(x) > 5)]
        #'
        #' @param columns May be character column names or numeric positions. See details.
        #' @param persist Whether the subset should persist after  evaluation.
        #'
        #' @details
        #'  The form startcol:endcol is also allowed. Dropping the specified columns can be accomplished by prepending the argument with ! or -, e.g. .SDcols = !c('x', 'y').
        #'  See documentation of `.SDcols` in `?data.table::data.table` for more possibilities.
        #'
        #' @return Invisibly returns itself.
        select = function(columns, persist=FALSE) {#browser()
            if (missing(columns)) columns <- NULL
            if (!is_true_or_false(persist)) stop("Persist must be either true (1) or false (0).")
            e <- substitute(columns)
            private$sdcols <- private$parse_sdcols(e, parent.frame())
            private$sdcols_txt <- deparse1(e)
            private$sdcols_persist <- persist
            private$sdcols_env = parent.frame()
            invisible(self)
        },

        #' @description Set column values.
        #' Experimental. #TODO define behaviour of other methods.
        #'
        #' @param value Value to assign to the columns and rows specified by where.
        #'
        #' @details
        #' TO ADD
        #'
        #' @examples
        #' df <- DataFrame$new(data.table(a=1:3, b=1:3, d = LETTERS[1:3]))
        #' df$select(is.character)$where(a==2)$set(a); df
        #' df$where(a==2)$set(fifelse(a==3, 1, 0)); df
        #' df$select(is.numeric)$set(NA); df
        #'
        #' @return Invisibly returns itself.
        set = function(value) {#browser()
            value <- substitute(function(x) value)
            cols <- if (is.null(private$sdcols)) names(private$tbl) else names(eval(private$tbl[i=0,.SD,.SDcols=private$sdcols]))
            j <- substitute(`:=` (cols, lapply(.SD, FUN=value)))
            private$tbl_eval(i=private$i, j=j, keyby=private$keyby, .SDcols=private$sdcols)
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
            !is.null(private$keyby)
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

        keyby = NULL,
        keyby_cols = NULL,
        keyby_persist = FALSE,

        i = NULL,
        i_txt = NULL,
        i_env = NULL,
        i_persist = FALSE,

        sdcols = NULL,
        sdcols_txt = NULL,
        sdcols_env = NULL,
        sdcols_persist = FALSE,


        tbl_subset = function() {
            e <- private$call(i=private$i, j=quote(.SD), .SDcols=private$sdcols)
            eval(e)
        },

        tbl_eval = function(i, j, keyby, .SDcols) {
            private$eval(private$call(i, j, keyby, .SDcols))
        },

        reset_i = function() {
            private$i  <- private$i_txt <- private$i_env <-  NULL
        },

        reset_sdcols = function() {
            private$sdcols  <- private$sdcols_txt <- private$sdcols_env <-  NULL
        },

        reset_keyby = function() {
            private$keyby  <- private$keyby_cols <- NULL
        },

        eval = function(e) {
            result <- eval(e)
            if (!private$i_persist) private$reset_i()
            if (!private$sdcols_persist) private$reset_sdcols()
            if (!private$keyby_persist) private$reset_keyby()
            result
        },

        call = function(i, j, keyby, .SDcols) {
            e <- quote(`[`(private$tbl))
            if (!missing(i) && !is.null(i)) e[["i"]] <- i
            if (!missing(j) && !is.null(j)) e[["j"]] <- j
            if (!missing(keyby) && !is.null(keyby)) e[["keyby"]] <- keyby
            if (!missing(.SDcols) && !is.null(.SDcols)) e[[".SDcols"]] <- .SDcols
            e
        },

        parse_i = function(e, env) {#browser()
            # e can be:
            # 1. name/symbol - is.name
            # 2. single character / string is.character, typeof = character
            # 3. a primitive/builtin - is.primitive `==`, `|`ˇ, c,... typeof == builtin
            # 3. a direct function call (e.g. grepl("a", b)) typeof closure, is.function
            # 4. a non built-in comparison (%in%,..) - syntactic sugar over function calls..typeof == closure
            # a combination of the above.

            # when a name evaluates to a vector (int, dbl, log, chr), the evaluated expr. should be kept unless it is a column nam
            # if e is of length 1 it can be either a name, an atomic vector of length one, a function without arguments

            if (is.null(e)) return(NULL)
            if (is.atomic(e)) return(e)
            if (is.name(e)) {
                col_check <- try(eval(e, private$tbl, emptyenv()), silent=TRUE)
                if (!inherits(col_check, "try-error")) return(e)

                ev <- eval(e, env) # note: if name is not found it will throw an error here
                if (is.atomic(ev)) return(ev)
                if (is.primitive(ev)) {
                    enq <- base::enquote(ev)
                    str <- paste0("`", sub('.*?\\"(.*)\\".*', "\\1", as.character(enq)[[2]]), "`")
                    return(str2lang(str))
                }

                e_chr <- as.character(e)
                return(substitute(get(e_chr, envir=private$i_env)))
            }

            if (length(e) < 2) stop("Error.")

            if (e[[1]] == quote(`$`) || e[[1]] == quote(`[`) || e[[1]] == quote(`[[`)) {
                e[[2]] <-  private$parse_expr(e[[2]], env)
                #TODO  the 3rd element should be evaluated inside the second element
                return(e)
            }

            fun <- eval(e[[1]])
            if (!is.function(fun)) stop("First element of expression must be a function!")
            fun_chr <- as.character(e[[1]])
            if (!is.primitive(fun)) e[[1]] <- substitute(get(fun_chr, envir=private$i_env))

            for (i in seq_along(e)[-1L]) {
                e[[i]] <- private$parse_i(e[[i]], env)
            }

            e

        },

        parse_sdcols = function(e, env) {#browser() #TODO
            # e can be:
            # 1. character column names or numeric positions - is.character, is.numeric
            # 2. startcol:endcol
            # 3. .SDcols=patterns(regex1, regex2, ...) - evaluated on names
            # 3. .SDcols=is.numeric - evaluated on columns
            # 4. Inversion (column dropping instead of keeping) can be accomplished be prepending the argument with ! or -

            if (is.null(e)) return(NULL)
            if (length(e) == 3 && e[[1]] == quote(`:`)) return(e)
            ev <- try(eval(e, env), silent=TRUE)
            if (!inherits(ev, "try-error")) {
                if (is.function(ev)) return(ev)
                if (is.character(ev) || is.numeric(ev)) return(ev)
            }

            if (length(e) < 2) stop("What now?")
            if (e[[1]] ==  quote(patterns)) return(e)
            if (e[[1]] == quote(`!`) || e[[1]] == quote(`-`)) {
                e[[2]] <- private$parse_sdcols(e[[2]], env)
                return(e)
            }

            stop("Do not know?")

        },

        parse_keyby = function(e) {
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

        rm_na_int = remove_na_integer,

        deep_clone = function(name, value) {
            if (name == "tbl") return(data.table::copy(value))
            value
        }
    )
)
