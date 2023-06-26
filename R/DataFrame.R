#' @title DataFrame Class
#'
#' @description A wrapper class for tabular data objects of class `data.frame`.
#'
#' @import data.table
#' @import R6
#' @include RcppExports.R
#' @include Call.R
DataFrame <- R6::R6Class(

    "DataFrame",

    cloneable=FALSE,

    public = list(
        #' @description `DataFrame` Constructor
        #'
        #' @param tbl An object of class `data.frame`.
        #' @param copy Optional argument specifying whether to wrap a copy of the passed object. Defaults to `FALSE`.
        #'
        #' @details The table is not copied by default which improves speed and memory performance.
        #' Potential drawback of not copying the table is the ability to modify the table 'in place' outside the wrapper, which results in modifying the wrapped table.
        initialize = function(tbl, copy=FALSE) {
            #CONSIDER adding ... as argument which will allow to create a DataFrame by passing vectors of same length: DF(1:5, LETTERS[1:5]).
            #CONSIDER allowing lists as in data.table construction
            stopifnot("tbl must be a data.frame" = inherits(tbl, "data.frame"))
            if (copy) {
                private$tbl <- data.table::as.data.table(tbl)
            } else {
                private$tbl <- data.table::setDT(tbl)
            }
            private$call <- Call$new()$use(private)
            invisible(self)
        },

        #' @description Print the table object.
        #'
        #' @nrows Even number of rows to print. Defaults to 12.
        #'
        #' @details The method used is `print.data.table`.
        #'
        print = function(nrows=12L) {
            caller_env <- if (length(sys.calls())==1) parent.frame() else parent.frame(3L)
            call <- private$call$clone()$subset(c("i",".SDcols"))$set(j=quote(.SD), env=caller_env)
            result <- call$eval(caller_env)
            private$print_header()
            print(result, nrows=nrows, topn=floor(nrows/2), class=TRUE, print.keys=TRUE)
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
            call <- private$call$clone()$subset(c("i",".SDcols"))$set(j=quote(.SD), env=parent.frame())
            result <- call$eval(parent.frame())
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
            call <- private$call$clone()$subset(c("i",".SDcols"))$set(j=quote(.SD), env=parent.frame())
            result <- call$eval(parent.frame())
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
            private$call$set(j=quote(list(.N)), env=parent.frame())
            result <- private$call$subset(c("i", "j", "by", "keyby"))$eval(parent.frame())
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
            private$call$set(i=substitute(rows), env=parent.frame())
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
        #' @param ... May be character column names or numeric positions. See details.
        #'
        #' @details
        #'  The form startcol:endcol is also allowed. Dropping the specified columns can be accomplished by prepending the argument with ! or -, e.g. .SDcols = !c('x', 'y').
        #'  See documentation of `.SDcols` in `?data.table::data.table` for more possibilities.
        #'
        #' @return Invisibly returns itself.
        select = function(...) {#browser()
            e <- if (missing(...))quote(list(NULL)) else substitute(list(...))
            private$call$set(j=quote(.SD), .SDcols=e, env=parent.frame())
            invisible(self)
        },

        #' @description Group or un-group the data.
        #' Used in calculation of statistics.
        #'
        #' @param ... An expression specifying by what to group the data. See details.
        #' @param .as_key Whether to use the grouping as a key of the resulting `DataFrame`.
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
            e <- substitute(list(...))
            if (.as_key) private$call$set(keyby=e, env=parent.frame()) else private$call$set(by=e, env=parent.frame())
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
            cols <- if (is.null(private$call$arg(".SDcols"))) names(private$tbl) else private$call$.SD_colnames(parent.frame())

            if (length(cols) == 0) {
                warning("No columns matching the select criteria!")
            } else {
                private$call$set(j = substitute(`:=` (cols, lapply(.SD, FUN=value))), env=parent.frame())
                private$call$eval(parent.frame())
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
            cols <- private$call$.SD_colnames(parent.frame())
            if (length(cols) == 0) {
                warning("No columns matching the select criteria!")
            } else {
                cols_call <- str2lang(paste0("c(", paste0("'", cols, "'", collapse = ","), ")"))
                private$call$set(j = substitute(`:=` (cols_call, lapply(.SD, fun, ...))), env=parent.frame())
                private$call$eval(parent.frame())
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
            j <- substitute(list(...))
            if (!all(names(j)[-1L] %in% names(private$tbl))) stop("Use the insert method to add new columns!")
            j[[1]] <- quote(`:=`)
            private$call$set(j=j, env=parent.frame())$subset(c("i", "j", "by", "keyby"))$eval(parent.frame())
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

            inner_j <- Call$new()$set(x=substitute(other), on=substitute(on), env=parent.frame())
            inner_j$reverse_on()
            ON_REV <- inner_j$arg("on")
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

            J <- inner_j$set(i=quote(.SD), j=J, mult="all", nomatch=NA, env=parent.frame())$get()

            private$call$set(j = substitute(`:=` (new_cols, J)), env=parent.frame())

            tryCatch(
                private$call$eval(parent.frame()),
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
            call <- Call$new()$set(x=quote(other), i=quote(private$tbl), on=substitute(on), mult="all", nomatch=NA, env=environment())
            ON <- call$arg("on")
            call$reverse_on()
            ON_REV <- call$arg("on")

            result <- call$eval(environment())
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
            private$call$set(j=e, env=parent.frame())$subset(c("i", "j", "by", "keyby"))
            private$call$eval(parent.frame())
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
            private$call$set(j=substitute(lapply(lapply(.SD, function(x) {fexpr}), unlist)), env=parent.frame())
            result <- private$call$eval(parent.frame())
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
            assign("call", private$call$clone(), .subset2(result, ".__enclos_env__")$private)
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
        }
    ),

    private = list(

        call = NULL,


        tbl = NULL,

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
            i <- if (!is.null(private$call$arg("i"))) {
                txt <- deparse1(private$call$arg("i"))
                paste("  Using rows where:", txt, "\n")
            } else NULL
            sdcols <- if (!is.null(private$call$arg(".SDcols"))) {
                txt <- deparse1(private$call$arg(".SDcols"))
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
            call <- private$call$clone()$subset(NULL)
            BY <- private$call$arg("by")
            if (!is.null(BY)) {
                tmp <- i
                i <- substitute(private$tbl[, .I[tmp], by=BY]$V1)
            }
            call$set(i=i, env=parent.frame(2L))$eval(parent.frame(2L))
        }
    )
)
