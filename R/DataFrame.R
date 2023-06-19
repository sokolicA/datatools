#' @title DataFrame Class
#'
#' @description A wrapper class for tabular data objects of class `data.frame`.
#'
#' @import data.table
#' @import R6
#' @include RcppExports.R
#' @include StaticEnv.R
#' @include DTCall.R
DataFrame <- R6::R6Class(

    "DataFrame",

    cloneable=FALSE,

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
            #CONSIDER add alias as suffix in joins
            stopifnot("tbl must be a data.frame" = inherits(tbl, "data.frame"))
            if (copy) {
                private$tbl <- data.table::as.data.table(tbl)
            } else {
                private$tbl <- data.table::setDT(tbl)
            }
            private$alias <- private$static_env$add(alias)
            private$call <- DTCall$new(private$tbl)
            invisible(self)
        },

        #' @description Print the table object.
        #'
        #' @details The method used is `print.data.table`.
        #'
        print = function() {#browser()
            private$print_header()
            result <- private$eval(private$call$copy(c("i",".SDcols"))$set(j=quote(.SD))$call(), reset=FALSE)
            print(result, class=TRUE)
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
            result <- private$eval(private$call$copy(c("i",".SDcols"))$set(j=quote(.SD))$call(), reset=FALSE)
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
            result <- private$eval(private$call$copy(c("i",".SDcols"))$set(j=quote(.SD))$call(), reset=FALSE)
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
            private$call$set(j=quote(list(.N)))
            result <- private$eval(private$call$call(c("i", "j", "by", "keyby")))
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
            result <- private$.filter(substitute(keep))
            DataFrame$new(result)
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
            private$tbl <- private$.filter(substitute(keep))
            invisible(self)
        },

        #' @description Work (operate?) on a subset of rows.
        #' Experimental.
        #' This method will not remove the rows from the data.
        #' Selected data modifications or calculations will be based only on the selected subset of data.
        #'
        #'
        #' @param rows An expression to be evaluated inside the table, integer vector specifying rows to remove or a logical vector.
        #'
        #' @details
        #' If `rows` is missing or `NULL` then all rows will be used.
        #'
        #' @return Invisibly returns itself.
        where = function(rows) {#browser()
            if (missing(rows)) rows <- NULL
            private$call$set(i=private$parse_i(substitute(rows), parent.frame()))
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
        #'
        #' @details
        #'  The form startcol:endcol is also allowed. Dropping the specified columns can be accomplished by prepending the argument with ! or -, e.g. .SDcols = !c('x', 'y').
        #'  See documentation of `.SDcols` in `?data.table::data.table` for more possibilities.
        #'
        #' @return Invisibly returns itself.
        select = function(columns) {#browser()
            if (missing(columns)) columns <- NULL
            e <- substitute(columns)
            sdcols <- private$parse_sdcols(e, parent.frame())
            test_call <- DTCall$new()$set(i=0, j=quote(.SD), .SDcols=sdcols)$call()
            private$eval(test_call, reset=FALSE)
            private$call$set(j=quote(.SD), .SDcols=sdcols)
            invisible(self)
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
        group_by = function(..., .as_key=FALSE) {
            e <- substitute(alist(...))[-1L]
            result <- private$parse_by(e)
            check <- try(eval(substitute(private$tbl[0][, .N, by = result])), silent=TRUE)
            if (inherits(check, "try-error")) stop(attr(check, "condition")$message)
            if (.as_key) private$call$set(keyby=result) else private$call$set(by=result)
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
            cols <- if (is.null(private$call$get(".SDcols"))) names(private$tbl) else private$.SD_colnames()

            if (length(cols) == 0) {
                warning("No columns matching the select criteria!")
            } else {
                private$call$set(j = substitute(`:=` (cols, lapply(.SD, FUN=value))))
                private$eval(private$call$call())
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
            cols <- private$.SD_colnames()
            if (length(cols) == 0) {
                warning("No columns matching the select criteria!")
            } else {
                cols_call <- str2lang(paste0("c(", paste0("'", cols, "'", collapse = ","), ")"))
                private$call$set(j = substitute(`:=` (cols_call, lapply(.SD, fun, ...))))
                private$eval(private$call$call())
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
            #TODO add tests
            j <- substitute(alist(...))
            if (!all(names(j)[-1L] %in% names(private$tbl))) stop("Use the insert method to add new columns!")
            j[[1]] <- quote(`:=`)
            private$eval(private$call$set(j=j)$call())
        },

        #' @description Perform an update join.
        #'
        #' @param other The other (right) `data.table`.
        #' @param on The condition to join the tables on. Either an unnamed character vector c("a") or a named character vector c(a="b") or a list list(a).
        #' @param insert Optional list specifying which columns to add. `NULL` (default) does not insert any. Use `'add'` to insert all columns from the other table that are not used for joining.
        #' @param update Optional list specifying updates of existing columns. No updates are performed by default.
        #'
        #' @return Invisibly returns the updated itself.
        #'
        #' Returns an error if there are multiple matches found in the `other` table.
        #'
        #' @details
        #'
        #' Using columns from the wrapped `data.table` table in calculations provided in `update` can be done by
        #' prefixing the column names of the left table with **`i.`**. See examples.
        #'
        #' Note that `list(...)` can be aliased with `.(...)` due to the background use of `data.table`.
        #'
        #' The join will not be performed if there are multiple matches found in the `other` table.
        #' In such cases use either `left_join` or delete duplicated foreign keys in the `other` table.
        #'
        #' @examples
        #' x <- data.table(a=1:3, b = c("a", "b", "a"))
        #' y <- data.table(a=c("b", "c", "a"), b = 5:7)
        #' df <- DF(x)
        #' df$update_join(y, .(b=a), insert="all")
        update_join = function(other, on, insert=NULL, update=NULL) {#browser()
            #CONSIDER To allow data.tables or only DataFrames or both?
            #REFACTOR
            insert <- substitute(insert)
            update <- substitute(update)
            if (!inherits(other, "data.table")) stop("Must provide a data.table object.")
            if (is.null(insert) && is.null(update)) {
                warning("Provide either insert or update!");
                return(invisible(self))
            }

            ON <- private$parse_on(substitute(on))
            ON_REV <- private$call$reverse_on(ON)
            J <- NULL

            if (is.null(insert)) {
                #Do nothing
            } else if (insert=="all") {# add all columns
                SDCOLS <- setdiff(names(other), names(ON_REV))
                new_cols <- private$rename_duplicated_cols(SDCOLS)
                J <- str2lang(paste0(".(", paste(SDCOLS, collapse = ","), ")"))
            } else {
                J <- private$add_missing_join_j_expr_names(insert)
                if (any(names(J)[-1L] %in% names(private$tbl))) stop("Some columns already exist! Provide new column names or use the update argument to update existing columns.", call.=FALSE)
                new_cols <- names(J)[-1L]
            }

            if (!is.null(update)) {
                if (is.null(names(update)) || any(names(update)[-1L] == "")) stop("Must provide column names to update!")
                if (!all(names(update)[-1L] %in% names(private$tbl))) stop("New columns must be provided to the insert argument!")
                J <- merge_calls(J, update)
                new_cols <- c(new_cols, names(update)[-1L])
            }

            inner_j <- DTCall$new()$set(x=substitute(other), i=quote(.SD), j=J, on = ON_REV,
                                        mult="all", nomatch=NA)$call()

            private$call$set(j = substitute(`:=` (new_cols, inner_j)))

            tryCatch(
                private$eval(private$call$call()),
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
        #' @param other The other (right) `data.table`.
        #' @param on The condition to join the tables on. Either a...
        #'
        #' @return A new `DataFrame` extended with columns from the other table.
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
        left_join = function(other, on) {#browser()
            ON <- private$parse_on(substitute(on))
            ON_REV <- private$call$reverse_on(ON)

            call <- DTCall$new()$set(x=substitute(other), i=quote(private$tbl), on=ON_REV, mult="all", nomatch=NA)
            result <- private$eval(call$call(), reset=FALSE)
            # x[y, on...]
            # if on column names differ, the column from y is not returned
            # duplicated columns in y are prefixed with i.
            # if y has a column of the same name of one of the on columns for x (but is not used in the join), it is also added and prefixed with i.
            on_x <- names(ON)[-1L]
            on_y <- names(ON_REV)[-1L]
            new_x <- old_x <- setdiff(names(private$tbl), on_x)
            new_y <- old_y <- setdiff(names(other), on_y)
            dupl <- new_x %in% names(other)
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
        #' #TODO add information about operations on groups - df$group_by(plate)$insert(test_1=mean(val[id=="CTRL"]))
        insert = function(...) {#browser()
            e <- substitute(list(...))
            if (is.null(names(e)) || any(names(e)[-1L]=="")) stop("Must pass named columns!")
            if (any(names(e) %in% names(private$tbl))) stop("Some columns already exist!")
            e[[1L]] <- quote(`:=`)
            private$call$set(j=e)
            private$eval(private$call$call())
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

        #' @description Data aggregation
        #'
        #' @param ... Functions used to create an aggregate summary. See details.
        #'
        #' @details
        #' An additional column with the name of the function is added to the table.
        #' Passing named arguments will result in using the names in the output. See examples.
        #'
        #'
        #' @return A `DataFrame`.
        #'
        #' @examples
        #' df <- DF(mtcars, copy=TRUE)
        #' sum_squares <- function(x) sum(x**2)
        #' df$aggregate(sum_squares(x), mean(x), sd(x))
        #' df$aggregate(max(x), mean(x))
        #' df$aggregate(mean(x), mean_na_rm = mean(x, na.rm=T))
        aggregate = function(...) {#browser()
            fexpr <- substitute(list(...))
            if (grepl("function\\(", deparse(fexpr))) stop("Anonymous functions are not supported!")
            groups <- private$call$grouping()
            private$call$set(j=substitute(lapply(lapply(.SD, function(x) {fexpr}), unlist)))
            result <- private$eval(private$call$call())
            private$finalize_aggregate(result, fexpr, groups)
            DF(result)
        },

        #' @description Get the underlying data.
        #'
        #' @return The underlying `data.table` object.
        #'
        unwrap = function() {
            private$tbl[]
        },

        #' @description Create a deep copy of the `DataFrame` object.
        #'
        #' @return A new `DataFrame` object.
        #'
        copy = function() {#browser()
            result <- DataFrame$new(tbl=private$tbl, copy=TRUE)
            assign("call", private$call$copy(), .subset2(result, ".__enclos_env__")$private)
            result
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

        .SD_colnames = function() {
            tmp <- private$call$copy(c(".SDcols"))$set(i=0, j=quote(.SD))$call()
            names(private$eval(tmp, reset=FALSE))
        },


        call = NULL,

        static_env = StaticEnv$new(),

        alias = NULL,

        tbl = NULL,

        i_env = NULL,

        eval = function(e, reset=TRUE) {
            env <- private$build_eval_env()
            result <- eval(e, envir=env, enclos=env)
            if (reset) private$call <- DTCall$new(private$tbl)
            result
        },

        build_eval_env = function() {
            result <- new.env(parent = private$caller_env())
            result$private <- private
            result
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
            # 2. atomic - null, character, integer,...
            # 3. a primitive: builtin (is.integer) or special (&)
            # 3. a direct function call (e.g. grepl("a", b)) typeof closure, is.function
            # 4. a non built-in comparison (%in%,..) - syntactic sugar over function calls..typeof == closure
            # 5. a combination of the above.
            if (is.atomic(e)) return(e)
            if (is.symbol(e)) {
                is_column <- !inherits(try(eval(e, private$tbl, emptyenv()), silent=TRUE), "try-error")
                if (is_column) return(e)

                ev <- try(eval(e, env), silent=TRUE) # note: if name is not found it will throw an error here
                if (inherits(ev, "try-error")) stop(attr(ev, "condition")$message, call.=FALSE)
                if (is.atomic(ev)) return(ev)
                if (is.function(ev)) stop("Error. When can this happen? Function without arguments")

                e_chr <- deparse1(e)
                e_env <- find_obj_env(e_chr, env)
                if (identical(e_env, globalenv()) || isNamespace(e_env)) return(e)
                warning("Using locally defined functions or variables!", call. = FALSE)
                return(substitute(get(e_chr, envir=private$i_env)))
            }

            if (length(e) < 2) stop("Unable to parse expression. Empty function call?")

            if (e[[1]] == quote(`$`) || e[[1]] == quote(`[`) || e[[1]] == quote(`[[`)) {
                e[[2]] <-  private$parse_i(e[[2]], env)
                #TODO  the 3rd element should be evaluated inside the second element
                return(e)
            }

            f <- eval(e[[1]], env)
            if (!is.function(f)) stop("First element of expression must be a function!", call.=FALSE)
            if (length(e[[1]]) > 1) stop("Currently unable to parse functions inside other objects.", call. = FALSE)


            if (!any(is.primitive(f), isNamespace(environment(f)), identical(environment(f), globalenv()))) {
                warning("Using locally defined functions or variables!", call. = FALSE)
                fun_chr <- deparse1(e[[1]])
                e[[1]] <- substitute(get(fun_chr, envir=private$i_env))
            }


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

            if (e[[1L]] != quote(c)) stop("'on' argument should be a named vector of column names indicating which columns in self should be joined with which columns in other.", call.=FALSE)
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
                # if (length(dupl)>0) {
                #     warning(paste("Column(s)", paste0(dupl, collapse=", "), "will be overwritten with columns of the same name from the other table."), call. = FALSE)
                # }
                names(e) <- result
            }
            e
        },

        rename_duplicated_cols = function(new_cols) {
            while (any(dupl <- new_cols %in% names(private$tbl))) {
                new_cols[dupl] <- paste0(new_cols[dupl], "_y")
            }
            new_cols
        },

        print_header = function() {
            d <- dim(private$tbl)
            title <- paste("Wrapping a", d[1], "x", d[2], "data.table.\n")
            group <- if (!is.null(private$call$grouping())) {
                txt <- gsub("(^list\\()|(\\)$)", "", deparse1(private$call$grouping()))
                paste("  Grouped by:", txt, "\n")
            } else NULL
            i <- if (!is.null(private$call$get("i"))) {
                txt <- deparse1(private$call$get("i"))
                paste("  Using rows where:", txt, "\n")
            } else NULL
            sdcols <- if (!is.null(private$call$get(".SDcols"))) {
                txt <- deparse1(private$call$get(".SDcols"))
                paste("  Columns subset using:", txt, "\n")
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
        },

        finalize_aggregate = function(result, f_expr, by_expr) {
            if (f_expr[[1]] == quote(list)) {
                f_expr <- f_expr[-1L]
                f_names <- if (!is.null(names(f_expr))) names(f_expr) else vector("character", length=length(f_expr))
                missing <- which(f_names=="")
                if (length(missing) > 0) {
                    for (i in missing) f_names[i] <- deparse(f_expr[[i]][[1]])
                }
            } else {f_names <- deparse(f_expr[[1]])}
            result[, fun := rep(f_names, times=dim(result)[1L] %/% length(f_names))]

            groups <- private$aggregate_add_names_by(by_expr)
            if (!is.null(groups)) {data.table::setnames(result, seq_along(groups), groups)}
            data.table::setcolorder(result, c(groups, "fun"))
        },

        aggregate_add_names_by = function(e) {
            if (is.null(e)) return(NULL)
            e <- e[-1L]
            result <- if (!is.null(names(e))) names(e) else vector("character", length=length(e))
            if (all(result!="")) return(result)
            missing <- which(result=="")
            for (i in missing) result[i] <- deparse(e[[i]])
            result
        },

        .filter = function(i) {
            call <- DTCall$new()
            BY <- private$call$get("by")
            if (!is.null(BY)) {
                tmp <- i
                i <- substitute(private$tbl[, .I[tmp], by=BY]$V1)
            }
            call$set(i=i)
            private$eval(e=call$call(), reset = FALSE)
        }

    )
)
