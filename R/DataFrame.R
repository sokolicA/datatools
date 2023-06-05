#' @title DataFrame Class
#'
#' @description A wrapper class for tabular data objects of class `data.frame`.
#'
#' @import data.table
#' @import R6
#' @include RcppExports.R
#' @include StaticEnv.R
DataFrame <- R6::R6Class(
    "DataFrame",
    public = list(
        #' @description `DataFrame` Constructor
        #'
        #' @param tbl An object of class `data.frame`.
        #' @param copy Optional argument specifying whether to wrap a copy of the passed object. Defaults to `FALSE`.
        #' @param alias Optional alias of the DataFrame.
        #'
        #' @details The table is not copied by default which improves speed and memory performance.
        #' Potential drawback of not copying the table is the ability to modify the table 'in place' outside the wrapper, which results in modifying the wrapped table.
        initialize = function(tbl, copy=FALSE, alias=NULL) {
            #CONSIDER adding ... as argument which will allow to create a DataFrame by passing vectors of same length: DF(1:5, LETTERS[1:5]).
            #CONSIDER allowing lists as in data.table construction
            #TODO add alias as suffix in joins
            stopifnot("tbl must be a data.frame" = inherits(tbl, "data.frame"))
            if (copy) {
                private$tbl <- data.table::as.data.table(tbl)
            } else {
                private$tbl <- data.table::setDT(tbl)
            }
            private$alias <- private$static_env$add(alias)
            invisible(self)
        },

        #' @description Print the table object.
        #'
        #' @details The method used is `print.data.table`.
        #'
        print = function() {
            private$print_header()
            print(private$tbl_subset(), class=TRUE)
            invisible(self)
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
            result <- private$tbl_eval(i=private$i,j=quote(list(.N)), keyby=private$by)
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
            invisible(self)
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
            result <- private$parse_by(e)
            check <- try(eval(substitute(private$tbl[0][, .N, by = result])), silent=TRUE)
            if (inherits(check, "try-error")) stop(attr(check, "condition")$message)
            private$by <- result
            private$by_persist <- persist
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
        where = function(rows, persist=FALSE) {#browser()
            if (missing(rows)) rows <- NULL
            if (!is_true_or_false(persist)) stop("Persist must be either TRUE or FALSE.")
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
            private$tbl_eval(i=0, j=quote(.SD), .SDcols=private$sdcols, reset=FALSE)
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
            #CONSIDER renaming the method due to it overriding the default $set (method) method.
            value <- substitute(function(x) value)
            if (is.null(private$sdcols)) {
                cols <- names(private$tbl)
            } else {
                cols <- names(private$tbl_eval(i=0,j=quote(.SD),.SDcols=private$sdcols, reset=FALSE))
            }
            if (length(cols) == 0) {
                warning("No columns matching the select criteria!")
            } else {
                j <- substitute(`:=` (cols, lapply(.SD, FUN=value)))
                private$tbl_eval(i=private$i, j=j, by=private$by, .SDcols=private$sdcols)
            }
            invisible(self)
        },

        #' @description Transform columns with function
        #'
        #' @param fun A function that is applied to the selected columns.
        #' @param ... Optional arguments that are passed to `fun`.
        #'#'
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #'    df <- DF(data.frame(a=1:5, b=1:5, c=c(1:4, NA)))
        #'    df$where(b>3)$select("a")$transform(function(x) x + 50)
        #'    df$where(b>3)$select("c")$transform(mean, na.rm=T)
        transform = function(fun, ...) {
            cols <- names(private$tbl_eval(i=0, j=quote(.SD), .SDcols=private$sdcols, reset=FALSE))
            if (length(cols) == 0) {
                warning("No columns matching the select criteria!")
            } else {
                cols_call <- str2lang(paste0("c(", paste0("'", cols, "'", collapse = ","), ")"))
                j <- substitute(`:=` (cols_call, lapply(.SD, fun, ...)))
                private$tbl_eval(i=private$i, j=j, by=private$by, .SDcols=private$sdcols)
            }
            invisible(self)
        },

        #' @description Update table columns by reference.
        #'
        #' @param ... Columns to update and the corresponding calculation.
        #'
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #'    df <- DF(data.frame(a=1:5, b=1:5))
        #'    df$update(a = 2)
        #'    df$update(g = a, dd = ifelse(a==2, b, 0))
        update = function(...) {#browser()
            j <- substitute(alist(...))
            if (!all(names(j)[-1L] %in% names(private$tbl))) stop("Use the insert method to add new columns!")
            j[[1]] <- quote(`:=`)
            private$tbl_eval(i=private$i, j=j, keyby=private$keyby)
        },

        #' @description Perform an update join.
        #'
        #' @param right The right (other) `data.table`.
        #' @param on The condition to join the tables on. Either a...
        #' @param update Optional list specifying which columns to add. Can also update existing columns. By default adds all columns from the right table. See details for more information.
        #'
        #' @return Invisibly returns the updated itself.
        #'
        #' Returns an error if there are multiple matches found in the `right` table.
        #'
        #' @details
        #'
        #' Using columns from the wrapped `data.table` table in calculations provided in `update` can be done by
        #' prefixing the column names of the left table with **`i.`**. See examples.
        #'
        #' Note that `list(...)` can be aliased with `.(...)` due to the background use of `data.table`.
        #'
        #' The join will not be performed if there are multiple matches found in the `right` table.
        #' In such cases use either `left_join` or delete duplicated foreign keys in the `right` table.
        #'
        #' @examples
        #' x <- data.table(a=1:3, b = c("a", "b", "a"))
        #' y <- data.table(a=c("b", "c", "a"), b = 5:7)
        #' df <- DF(x)
        #' df$update_join(y, .(b=a))
        update_join = function(right, on, update=NULL) {#browser()
            #CONSIDER To allow data.tables or only DataFrames or both?
            #CONSIDER split update into insert and update. insert for new columns and update for existing.
            #REFACTOR
            if (!inherits(right, "data.table")) stop("Must provide a data.table object.")

            ON <- private$parse_on(substitute(on))
            ON_REV <- private$reverse_on_expr(ON)
            update <- substitute(update)

            if (is.null(update)) {# add all columns
                SDCOLS <- setdiff(names(right), names(ON_REV))
                J <- if (length(SDCOLS) > 1) quote(.SD) else quote(unlist(.SD, recursive = FALSE)) # a one dimensional list goes through the join without errors (when multiple matches), creating a multi-column column
                new_cols <- SDCOLS
                new_cols <- private$rename_duplicated_cols(new_cols)
                inner_j <- private$call(
                    substitute(`[`(right, i=.SD, mult="all", nomatch=NA)),
                    j=J, .SDcols=SDCOLS, on = ON_REV
                )
            } else {
                J <- update
                J <- private$add_missing_join_j_expr_names(J)
                inner_j <- private$call(
                    substitute(`[`(right, i=.SD, mult="all", nomatch=NA)),
                    j=J, on = ON_REV
                )
                new_cols <- names(J)[-1L]
            }

            tryCatch(
                private$tbl_eval(i=private$i, j = substitute(`:=` (new_cols, inner_j))),
                error = function(e) {
                    if (grepl("Supplied [1-9]+ items to be assigned to [1-9]+ items", e)) {
                        stop ("Unable to perform update join (by reference) due to the specified relationship resulting in a one to many join.", call.=FALSE)
                    }
                    if (grepl("argument specifying columns specify non existing column\\(s\\)", e)) {
                        col_name <- gsub(".*=", "", e$message)
                        param <- gsub(".*unname\\((.*)\\),.*", "\\1", deparse1(e$call))
                        msg <- paste0("Can not find column ", col_name, " provided in the '", param, "' argument.")
                        stop (msg, call.=FALSE)
                    }
                    if (identical(e$call, quote(eval(jsub, SDenv, parent.frame())))) {
                        col_name <- gsub(".*'(.*)'.*", "\\1", e$message)
                        msg <- paste0("Cannot find column '", col_name, "'.")
                        stop (msg, call.=FALSE)
                    }
                    stop(e)
                }
            )

            return(invisible(self))
        },

        #' @description Perform a left (outer) join.
        #'
        #' @param right The right (other) `data.table`.
        #' @param on The condition to join the tables on. Either a...
        #'
        #' @return A new `DataFrame` extended with columns from the right table.
        #'
        #'
        #' @details
        #'
        #' Note that `list(...)` can be aliased with `.(...)` due to the background use of `data.table`.
        #'
        #'
        #' @examples
        #' x <- data.table(a=1:3, b = c("a", "b", "a"))
        #' y <- data.table(a=c("b", "c", "a"), b = 5:7)
        #' df <- DF(x)
        #' df$left_join(y, .(b=a))
        left_join = function(right, on) {#browser()
            ON <- private$parse_on(substitute(on))
            ON_REV <- private$reverse_on_expr(ON)
            call <- private$call(
                e=substitute(`[`(right, mult = "all", nomatch = NA)),
                i=quote(private$tbl), on=ON_REV
            )
            result <- private$eval(call)
            # x[y, on...]
            # if on column names differ, the column from y is not returned
            # duplicated columns in y are prefixed with i.
            # if y has a column of the same name of one of the on columns for x (but is not used in the join), it is also added and prefixed with i.
            on_x <- names(ON)[-1L]
            on_y <- names(ON_REV)[-1L]
            new_x <- old_x <- setdiff(names(private$tbl), on_x)
            new_y <- old_y <- setdiff(names(right), on_y)
            dupl <- new_x %in% names(right)
            dupl_y <- old_y %in% c(old_x[dupl], on_x)
            old_x[dupl] <- paste0("i.", old_x[dupl])
            new_y[dupl_y] <- paste0(old_y[dupl_y], "_y")

            data.table::setnames(
                result,
                c(on_y, old_y, old_x),
                c(on_x, new_y, new_x)
            )
            setcolorder(result, names(private$tbl))
            DF(result)
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
            idx_remove <- int_remove_na(idx_remove)
            removed <- DataFrame$new(private$tbl[idx_remove])
            private$tbl <- private$tbl[!idx_remove]
            return(removed)
        },

        #' @description Insert columns to the `DataFrame`.
        #'
        #' Experimental. Considerations:
        #' This method will insert new columns by reference.
        #'
        #' @param ... Columns to add.
        #'
        #' @details
        #'  #'TO ADD
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #' TODO
        insert = function(...) {#browser()
            e <- substitute(alist(...))
            if (is.null(names(e)) || any(names(e)[-1L]=="")) stop("Must pass named columns!")
            if (any(names(e) %in% names(private$tbl))) stop("Some columns already exist!")
            e[[1L]] <- quote(`:=`)
            private$tbl_eval(i=private$i, j=e, keyby=private$keyby)
            invisible(self)
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
        #' res <- df$concat(y, x, y, df)
        concat = function(..., fill=FALSE) {
            to_concat <- list(private$tbl, ...)
            tbls <- lapply(to_concat, function(x) {
                if(inherits(x, "DataFrame")) return(x$unwrap())
                return(x)
            })
            DF(rbindlist(tbls, use.names = TRUE, fill = fill))
        },

        #' @description Concatenate the rows of the `DataFrame`.
        #' Same as the `$concat` method but without creating a new object.
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
        #' df$concat_(y, x, y, df)
        #'
        concat_ = function(..., fill=FALSE) {browser()
            private$tbl <- self$concat(..., fill=fill)$unwrap()
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
        },

        #' @field static Static Information
        static = function() {
            invisible(gc())
            private$static_env$info()
        }
    ),

    private = list(

        static_env = StaticEnv$new(),

        alias = NULL,

        tbl = NULL,

        by = NULL,
        by_cols = NULL,
        by_persist = FALSE,

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

        tbl_eval = function(i=NULL, j=NULL, by=NULL, keyby=NULL,
                            .SDcols=NULL, on=NULL, reset=TRUE) {
            private$eval(private$call(i=i, j=j, by=by, keyby=keyby, .SDcols=.SDcols, on=on), reset)
        },

        reset_i = function() {
            private$i  <- private$i_txt <- private$i_env <-  NULL
        },

        reset_sdcols = function() {
            private$sdcols  <- private$sdcols_txt <- private$sdcols_env <-  NULL
        },

        reset_by = function() {
            private$by  <- private$by_cols <- NULL
        },

        eval = function(e, reset=TRUE) {
            env <- private$build_eval_env()
            result <- eval(e, envir=env, enclos=env)
            if (reset) {
                if (!private$i_persist) private$reset_i()
                if (!private$sdcols_persist) private$reset_sdcols()
                if (!private$by_persist) private$reset_by()
            }
            result
        },

        call = function(
        e=quote(`[`(private$tbl)), i=NULL, j=NULL,
        by=NULL, keyby=NULL,
        .SDcols=NULL, on=NULL
        ) {
            if (!is.null(i)) e[["i"]] <- i
            if (!is.null(j)) e[["j"]] <- j
            if (!is.null(by)) e[["by"]] <- by
            if (!is.null(keyby)) e[["keyby"]] <- keyby
            if (!is.null(.SDcols)) e[[".SDcols"]] <- .SDcols
            if (!is.null(on)) e[["on"]] <- on
            e
        },

        build_eval_env = function() {
            env <- private$caller_env()
            env$private <- private
            env
        },

        caller_env = function() {
            i <- 2L
            while (identical(parent.env(parent.frame(i)), self$.__enclos_env__)) {
                i <- i + 1L
            }
            return(parent.frame(i))
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

            # add check for rows like private$tbl[0][a > 1e+05 & b < 14]?
            if (is.null(e)) return(NULL)
            if (is.atomic(e)) return(e)
            if (is.name(e)) {
                col_check <- try(eval(e, private$tbl, emptyenv()), silent=TRUE)
                if (!inherits(col_check, "try-error")) return(e)

                ev <- try(eval(e, env), silent=TRUE) # note: if name is not found it will throw an error here
                if (inherits(ev, "try-error")) stop(attr(ev, "condition")$message, call.=FALSE)
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

        parse_by = function(e) {
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

        parse_on = function(e) {
            # an unnamed character vector, c("a", "b"), used when columns a and b are common to both X and Y.
            # a named character vector, c(x1="y1", x2="y2"), used when join columns have different names (Foreign key joins).
            # string with a binary operator is also possible: c("x1==y1", "x2==y2")
            # a list (or it's dot alias): list(a, b), list(x1=y1, x2=y2)
            # non-equi joins are also possible: c("x>=a", "y<=b"), .(x>=a, y<=b)

            # CHECK .parse_on https://github.com/Rdatatable/data.table/blob/master/R/data.table.R
            if (is.null(e)) return(NULL)

            if (is.character(e)) {
                result <- call("c", e)
                names(result) <- c("", e)
                return(result)
            }

            if (e[[1L]] == quote(list) || e[[1L]] == quote(.)) {
                e <- private$convert_on_call(e)
            }

            if (e[[1L]] != quote(c)) stop("'on' argument should be a named vector of column names indicating which columns in self should be joined with which columns in right.", call.=FALSE)
            private$add_missing_on_expr_names(e)
        },

        convert_on_call = function(e) {
            ops <- c("==", "<=", "<", ">=", ">", "!=")
            pat <- paste0("(", ops, ")", collapse="|")
            spat <- paste0("[ ]+(", pat, ")[ ]+")
            # remove spaces around ==, >=,..
            e <- lapply(as.list(e)[-1L], function(x) gsub(spat, "\\1", deparse(x, width.cutoff=500L)))
            as.call(c(quote(c), e))
        },

        add_missing_on_expr_names = function(e) {
            result <- if (!is.null(names(e))) names(e) else vector("character", length=length(e))
            missing <- which(result == "")[-1L]
            if (any(missing)) {
                for (i in missing) result[i] <- sub("(=|!|>|<).*", "", e[[i]])
                names(e) <- result
            }
            e
        },

        add_missing_join_j_expr_names = function(e) {
            result <- if (!is.null(names(e))) names(e) else vector("character", length=length(e))
            missing <- which(result == "")[-1L]
            if (any(missing)) {
                for (i in missing) {
                    result[i] <- as.character(e[[i]])
                    e[[i]] <- as.name(paste0("x.", e[[i]]))
                }
                dupl <- intersect(result[missing], names(private$tbl))
                if (length(dupl)>0) {
                    warning(paste("Column(s)", paste0(dupl, collapse=", "), "will be overwritten with columns of the same name from the right table."), call. = FALSE)
                }
                names(e) <- result
            }
            e
        },

        reverse_on_expr = function(e) {
            result <- e
            result_names <- names(e)

            before <- c(">=", "<=", ">", "<", "!=")
            after <- c( "<", ">", "<=", ">=", "!=")

            for (i in seq_along(result)[-1L]) {
                val <- result[[i]]
                special <- sapply(before, grepl, val)
                if (any(special)) {
                    idx <- which.max(special)
                    res <- unlist(strsplit(val, before[idx]))
                    val <- res[2]
                    res <- paste0(res[2], after[idx], res[1])
                    result_names[i] <- res
                }
                result[[i]] <- result_names[i]
                result_names[i] <- val
            }
            names(result) <- result_names
            result
        },

        rename_duplicated_cols = function(new_cols) {
            while (any(dupl <- new_cols %in% names(private$tbl))) {
                new_cols[dupl] <- paste0(new_cols[dupl], "_y")
            }
            new_cols
        },

        deep_clone = function(name, value) {
            if (name == "tbl") return(data.table::copy(value))
            value
        },

        print_header = function() {
            d <- dim(private$tbl)
            title <- paste("Wrapping a", d[1], "x", d[2], "data.table.\n")
            group <- if (!is.null(private$by)) {
                paste("  Grouped by:", gsub("(^list\\()|(\\)$)", "", deparse1(private$by)), "\n")
            } else NULL
            i <- if (!is.null(private$i)) {
                paste("  Using rows where:", private$i_txt, "\n")
            } else NULL
            sdcols <- if (!is.null(private$sdcols)) {
                paste("  Columns subset using:", private$sdcols_txt, "\n")
            } else NULL
            length <- max(sapply(c(title, group, i, sdcols), nchar)) - 1
            divisor <- paste0(rep("-", times = length), collapse="")
            cat(title)
            cat(i)
            cat(sdcols)
            cat(group)
            cat(divisor, "\n")
        },

        finalize = function() {
            private$static_env$remove(private$alias)
        }

    )
)
