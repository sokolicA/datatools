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
        #' @description `DataFrame` Constructor.
        #'
        #' @param tbl An object of class `data.frame`.
        #' @param copy Optional argument specifying whether to wrap a copy of the passed object. Defaults to `FALSE`. See details.
        #'
        #' @details The table is not copied by default for speed and memory performance.
        #' Potential drawback of not copying the table is the ability to modify the table 'in place' outside the wrapper, which results in modifying the wrapped table.
        initialize = function(tbl, copy=FALSE) {
            #CONSIDER adding ... as argument which will allow to create a DataFrame by passing vectors of same length: DF(1:5, LETTERS[1:5]).
            #CONSIDER allowing lists as in data.table construction
            if (!inherits(tbl, "data.frame")) stop("tbl must be a data.frame!")
            private$tbl <- if (copy) data.table::as.data.table(tbl) else data.table::setDT(tbl)
            private$call <- Call$new(tbl_env=private, depth=2L)
            invisible(self)
        },

        #' @description Get the underlying data.
        #'
        #' @return The underlying `data.table` object.
        #'
        unwrap = function() {
            private$tbl[]
        },

        #' @description Print the table object.
        #'
        #' @param nrows Even number of rows to print. Defaults to 12.
        #'
        #' @details The output is affected by the setup verbs `where` and `select`.
        #' The header of the output is information about the `DataFrame`.
        #' The method used to print the table is `print.data.table` using custom arguments.
        #'
        print = function(nrows=12L) {#browser()
            n_calls <- length(sys.calls())
            caller_env <- if (n_calls==1) parent.frame() else sys.frame(n_calls-1) # df$print() or df
            call <- private$call$clone()$subset(c("i",".SDcols"))$set(j=quote(.SD), env=caller_env)
            result <- call$eval(caller_env)
            private$print_header()
            print(result, nrows=nrows, topn=floor(nrows/2), class=TRUE, print.keys=TRUE)
            invisible(self)
        },

        #' @description Create a deep copy of the `DataFrame` object.
        #'
        #' @return A new `DataFrame` object.
        #'
        copy = function() {#browser()
            DataFrame$new(tbl=private$tbl, copy=TRUE)
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
            call <- private$call$clone()$subset(c("i",".SDcols"))$set(j=quote(.SD))$eval()
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
            result <- private$call$clone()$subset(c("i",".SDcols"))$set(j=quote(.SD))$eval()
            DataFrame$new(tail(result, n))
        },

        #' @description Create a new `DataFrame` with filter applied to the rows.
        #'
        #' @param keep An expression to be evaluated inside the table, integer vector specifying rows to remove or a logical vector. See details.
        #'
        #' @details
        #' \itemize{
        #' \item If an expression is passed it will be evaluated inside the context of the table.
        #' \item If an integer vector is passed, the rows specified will be kept. Passing duplicated numbers will result in duplicated rows and passing numbers larger than the number of rows will result in `NA` rows.
        #' \item If a logical vector is passed it must either of length one the same length as the number of rows. Logical `NA` values are treated as `FALSE` and those rows will not be returned.
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
        #' Same as `$filter` method, just done in place.
        #'
        #' @param keep An expression to be evaluated inside the table, integer vector specifying rows to remove or a logical vector. See details of `$filter`.
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
            remove_idx <- Call$new(private, depth=1L)$set(j = substitute(.I[where]))$eval()
            remove_idx <- int_remove_na(remove_idx)
            removed <- DataFrame$new(private$tbl[remove_idx])
            private$tbl <- private$tbl[!remove_idx]
            return(removed)
        },

        #' @description Create a new `DataFrame` by concatenating table rows.
        #'
        #' @param ... Objects of class `data.frame` or `DataFrame`.
        #' @param fill Optional parameter whether to fill missing columns with `NA`. Defaults to `FALSE`.
        #'
        #' @return A new unkeyed `DataFrame` object with rows from tables passed in `...` appended.
        #'
        #' @details
        #' The concatenation is based on column names.
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
                x
            })
            DataFrame$new(rbindlist(tbls, use.names = TRUE, fill = fill))
        },

        #' @description Concatenate the rows of the `DataFrame` (in place).
        #' The result of `$concat` method is set as the new underlying table.
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
        concat_ = function(..., fill=FALSE) {#browser()
            private$tbl <- self$concat(..., fill=fill)$unwrap()
            invisible(self)
        },

        #' @description Perform a left (outer) join.
        #'
        #' @param other The other `data.table`.
        #' @param on The condition to join the tables on. Either an unnamed character vector c("a") or a named character vector c(a="b") or a list list(a).
        #'
        #' @return A new `DataFrame` extended with columns from the other table.
        #'
        #'
        #' @details
        #' The method performs a full left outer join.
        #'
        #' @examples
        #' x <- data.table(a=1:3, b = c("a", "b", "a"))
        #' y <- data.table(a=c("b", "c", "a"), b = 5:7)
        #' df <- DF(x)
        #' df$left_join(y, .(b=a))
        left_join = function(other, on) {#browser()
            call <- Call$new()$set(
                x=quote(other), i=quote(private$tbl),
                on=substitute(on), mult="all", nomatch=NA, env=environment()
            )

            ON <- call$arg("on")
            call$reverse_on()
            ON_REV <- call$arg("on")

            result <- call$eval(environment())
            private$l_join_rename(result, names(ON)[-1L], names(ON_REV)[-1L], names(other))
            setcolorder(result, names(private$tbl))
            DataFrame$new(result)
        },

        #' @description Operate on a subset of rows.
        #'
        #' Part of the *setup methods*.
        #'
        #' This method sets the `i` argument of the `data.table` call without evaluating the call.
        #' Eligible data modifications (*update methods*) or calculations
        #' (*transformation methods*) will be based only on the used subset of data
        #' and using them will entirely reset the *setup methods*.
        #'
        #' Note that this is fundamentally different than the `$filter` method, which
        #' evaluates and returns the subset.
        #'
        #' @param rows An expression to be evaluated inside the table,
        #' integer vector specifying rows to remove or a logical vector.
        #'
        #' @details
        #' If `rows` is missing or `NULL` then all rows will be used.
        #'
        #' Experimental: **Subsetting by group.** If grouping is already specified when calling `where`, the
        #' subsetting will be done by group and the `group_by` setup will be consumed. See examples.
        #'
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #' df <- DF(mtcars, copy=TRUE)
        #' df$where(mpg > 20)$print()
        #' df$group_by(vs)$where(mpg == max(mpg))$print()
        where = function(rows) {#browser()
            if (missing(rows)) rows <- NULL
            BY <- private$call$arg("by")
            i <- substitute(rows)
            if (!is.null(BY)) {
                i <- substitute(.__private__$tbl[, .I[i], by=BY]$V1)
                private$call$set(by=NULL, keyby=NULL)
            }
            private$call$set(i=i)
            invisible(self)
        },

        #' @description Operate on a subset of the columns.
        #'
        #' Part of the *setup methods*. Sets the `.SDcols` argument.
        #'
        #' Eligible data modifications (*update methods*) or calculations
        #' (*transformation methods*) will be based only on the used subset of data
        #' and using them will entirely reset the *setup methods*.
        #'
        #' Note that this method will not remove the columns from the data. Use `$columns$drop` instead.
        #'
        #' @param ... May be character vector of column names or numeric positions. See details.
        #'
        #' @details
        #'  The form startcol:endcol is also allowed. Dropping the specified columns can be
        #'  accomplished by prepending the argument with ! or -, e.g. .SDcols = !c('x', 'y').
        #'  See documentation of `.SDcols` in `?data.table::data.table` for more possibilities.
        #'
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #'
        #' df <- DF(mtcars, copy=TRUE)
        #' df$select(c("mpg", "cyl"))$print()
        #' df$select(1:2)$print()
        #' df$select(is.numeric)$print()
        #' df$select(is.character)$print()
        #' df$select(patterns("m"))$print()
        select = function(...) {#browser()
            #IDEA $select(mean(x) >5) --> df[, .SD, .SDcols = sapply(df, function(x) mean(x) > 5)]
            #IDEA $select(mean(is.na(x)) >0.2) --> df[, .SD, .SDcols = sapply(df, function(x) mean(x) > 5)]
            e <- if (missing(...))quote(list(NULL)) else substitute(list(...))
            private$call$set(j=quote(.SD), .SDcols=e)
            invisible(self)
        },

        #' @description Operate on groups of data.
        #'
        #' Part of the *setup methods*. Sets the `by` or `keyby` argument.
        #'
        #' Eligible data modifications (*update methods*) or calculations
        #' (*transformation methods*) will be group based
        #' and using them will entirely reset the *setup methods*.
        #'
        #' @param ... An expression specifying by what to group the data. See details.
        #' @param .as_key Whether to use the grouping as a key of the resulting `DataFrame`.
        #'
        #' @details
        #' Using `.as_key=TRUE` is suggested when using *transformation methods* but
        #' usually discouraged using *update methods*.
        #'
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
            if (.as_key) private$call$set(keyby=e) else private$call$set(by=e)
            invisible(self)
        },

        #' @description Reset the setup prepared with setup methods.
        #'
        #' Part of the *setup methods*. Resets the setup.
        #'
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #' df <- DF(mtcars, copy=TRUE)
        #' df$where(mpg>15)$print()
        #' df$reset_setup()$print()
        reset_setup = function() {
            private$call$subset(NULL)
            invisible(self)
        },

        #' @description Set column values.
        #'
        #' Part of the *update methods*. Uses `where`, `select` and `group_by`.
        #'
        #' @param value Value to assign. Can also be a function. See examples.
        #'
        #' @details
        #' If `where` is not set then all rows will be set to the value.
        #' If `select` is not set then all columns will be set to the value.
        #'
        #' @examples
        #' df <- DataFrame$new(data.table(a=1:3, b=1:3, d = LETTERS[1:3]))
        #' df$select(is.character)$where(a==2)$set(a)$print()
        #' df$where(a==2)$set(fifelse(a==3, 1, 0))$print()
        #' df$select(is.numeric)$set(NA)$print()
        #'
        #' df <- DF(mtcars, copy=FALSE)
        #' df$insert(test=1)$group_by(vs)$select("test")$set(max(mpg))$print()
        #'
        #' @return Invisibly returns itself.
        set = function(value) {#browser()
            #CONSIDER renaming the method due to it overriding the default $set (method) method.
            value <- substitute(function(x) value)
            cols <- if (is.null(private$call$arg(".SDcols"))) names(private$tbl) else private$call$selected_columns()

            if (length(cols) == 0) {
                warning("No columns matching the select criteria!")
            } else {
                private$call$set(j = substitute(`:=` (cols, lapply(.SD, FUN=value))))$eval()
            }
            invisible(self)
        },

        #' @description Insert (add) new columns to the `DataFrame`.
        #'
        #' Part of the *update methods*. Uses `where`, `select` and `group_by`.
        #'
        #' @param ... Named arguments in the form `column_name` = `expression`. See examples.
        #'
        #' @details
        #' This method can only be used to insert new columns and will throw an error
        #' if any of the columns are already found in the table.
        #' The `$update` method is intended for adding new columns.
        #'
        #' You can also use filtering on `j` with this method. This means that the whole table is
        #' taken but the operations are done only on a subset of the data in `.SD`.
        #' See here https://stackoverflow.com/questions/19847121/using-data-table-to-aggregate and
        #' in the examples.
        #' This is also an option with `$update`.
        #'
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #' df <- DF(mtcars, copy=TRUE)
        #' df$insert(mpg2 = mpg**2)
        insert = function(...) {#browser()
            e <- substitute(`:=`(...))
            if (any_unnamed(e)) stop("Must pass named columns!")
            if (any(names(e) %in% names(private$tbl))) stop("Some columns already exist!")
            private$call$set(j=e)$subset(c("i", "j", "by", "keyby"))$eval()
            invisible(self)
        },

        #' @description Update table columns by reference.
        #'
        #' Part of the *update methods*. Uses `where`, `select` and `group_by`.
        #'
        #' @param ... Named arguments in the form `column_name` = `expression`. See examples.
        #'
        #' @details
        #' This method can only be used to update existing columns and will throw an error
        #' if any of the columns are not found in the table.
        #' The `$insert` method is intended for adding new columns.
        #'
        #' You can also use filtering on `j` with this method. This means that the whole table is
        #' taken but the operations are done only on a subset of the data in `.SD`.
        #' See here https://stackoverflow.com/questions/19847121/using-data-table-to-aggregate and
        #' in the examples.
        #' This is also an option with `$insert`.
        #'
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #'    df <- DF(data.frame(a=1:5, b=1:5))
        #'    df$update(a = 2)
        #'    df$update(g = a, dd = ifelse(a==2, b, 0))
        #'
        #'   # Filtering on i (first transform) vs. filtering on j (second transform)
        #'    df <- DF(mtcars, copy=TRUE)
        #'    df$insert(a=NA_real_)$where(vs!=0)$group_by(cyl)$update(a= max(mpg))
        #'    df$insert(b=NA_real_)$group_by(cyl)$update(b= max(mpg[vs!=0]))
        update = function(...) {#browser()
            #TODO add tests
            j <- substitute(`:=`(...))
            if (!all(names(j)[-1L] %in% names(private$tbl))) {
                stop("Use the insert method to add new columns!")
            }
            private$call$set(j=j)$subset(c("i", "j", "by", "keyby"))$eval()
        },

        #' @description Perform an update join.
        #'
        #' Part of the *update methods*. Uses `where`.
        #'
        #' @param other The other `data.table`.
        #' @param on The condition to join the tables on. Either an unnamed character vector c("a") or a named character vector c(a="b") or a list list(a).
        #' @param insert Optional list specifying which columns to add. `NULL` (default) does not insert any. Use `'all'` to insert all columns from the other table that are not used for joining.
        #' @param update Optional list specifying updates of existing columns. No updates are performed by default.
        #'
        #' @return Invisibly returns the updated itself.
        #'
        #' In case of multiple matches in the `other` table the update by reference
        #' can not be performed and an error is thrown.
        #' In such cases use either `left_join` or delete duplicated foreign keys in the `other` table.
        #'
        #' @details
        #'
        #' Using columns from the wrapped `data.table` table in calculations provided in `update` can be done by
        #' prefixing the column names of the left table with **`i.`**. See examples.
        #'
        #' @examples
        #' x <- data.table(a=1:3, b = c("a", "b", "a"))
        #' y <- data.table(a=c("b", "c", "a"), b = 5:7)
        #' df <- DF(x)
        #' df$update_join(y, .(b=a), insert="all")
        update_join = function(other, on, insert=NULL, update=NULL) {#browser()
            #CONSIDER To allow data.tables or only DataFrames or both?
            #REFACTOR
            ins <- substitute(insert)
            upd <- substitute(update)
            if (!inherits(other, "data.table")) stop("Must provide a data.table object.")
            if (is.null(ins) && is.null(upd)) return(invisible(self))
            if (!is.null(upd)) {
                if (any_unnamed(upd)) stop("Must provide column names to update!")
                if (!all(names(upd)[-1L] %in% names(private$tbl))) stop("New columns must be provided to the insert argument!")
                upd[[1L]] <- quote(list)
            }

            rev_join <- Call$new()$set(x=substitute(other), on=substitute(on))$reverse_on()

            if (!is.null(ins)) {
                if (ins=="all") {
                    ins_cols <- setdiff(names(other), names(rev_join$arg("on")))
                    ins <- str2lang(paste0("list(", paste(ins_cols, collapse = ","), ")"))
                    ins_names <- private$upd_join_rename_duplicated_cols(ins_cols)
                } else {
                    ins[[1L]] <- quote(list)
                    ins <- private$upd_join_add_missing_insert_names(ins)
                    ins_names <- names(ins)[-1L]
                    if (any(ins_names %in% names(private$tbl))) stop("Some columns already exist! Provide new column names or use the update argument to update existing columns.", call.=FALSE)
                }
            }

            ins_upd <- merge_calls(ins, upd)
            new_names <- c(ins_names, names(upd)[-1L])
            J <- rev_join$set(i=quote(.SD), j=ins_upd, mult="all", nomatch=NA)$get()

            tryCatch(
                private$call$set(j = call(":=", new_names, J))$eval(),
                error = private$upd_join_error_handler
            )

            invisible(self)
        },

        #' @description Transform columns with function
        #'
        #' Part of the *update methods*. Uses `where`, `select` and `group_by`.
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
        transform = function(fun, ...) {#browser()
            cols <- private$call$selected_columns()
            if (length(cols) == 0) {
                warning("No columns matching the select criteria!")
            } else {
                J <- call(":=", cols, substitute(lapply(.SD, fun, ...)))
                private$call$set(j = J)$eval()
            }
            invisible(self)
        },


        #' @description Aggregate data.
        #'
        #' Part of the *transformation methods*. Uses `where`, `select` and `group_by`.
        #'
        #' @param ... Functions used to create an aggregate summary. See details.
        #'
        #' @details
        #' An additional column with the name of the function is added to the resulting table.
        #' Passing named arguments will result in using the names in the output. See examples.
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
            f <- substitute(list(...))
            if (grepl("function\\(", deparse(f))) stop("Anonymous functions are not supported!")
            groups <- private$call$grouping()
            private$call$set(j=substitute(lapply(lapply(.SD, function(x) {f}), unlist)))
            result <- private$call$eval()
            private$finalize_aggregate(result, f, groups)
            DataFrame$new(result)
        },

        #' @description Count the number of rows.
        #'
        #' @details Affected by the setup verbs `where` and `group_by`.
        #'
        #' @return A `DataFrame` with the row counts.
        #'
        #' @examples
        #' df <- DF(data.frame(x=1:5, g = c("a", "a", "b", "c", "c")))
        #' df$count()
        #' df$group_by(g)$count()
        count = function() {
            result <- private$call$subset(c("i", "by", "keyby"))$set(j=quote(list(.N)))$eval()
            DataFrame$new(result)
        },

        #' @description Order the table rows by column values.
        #'
        #' @param ...  The columns to sort by. Do not quote column names. See `?data.table::setorder`.
        #'
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #'    df <- DF(data.frame(a=1:5, b=1:5))
        #'    df$sort(-b)
        order_by = function(...) {
            if (!is.null(self$key)) stop("Table is already sorted with key!")
            data.table::setorder(private$tbl, ...)
            return(invisible(self))
        },

        #' @description Create a key on the table.
        #'
        #' `set_key` sorts the table using `data.table::setkeyv`, which marks it as sorted with an attribute sorted.
        #'
        #' @param ... Column names. Can either be quoted (strings) or unquoted.
        #'
        #' @return Invisibly returns itself.
        #' df <- DF(mtcars, copy=TRUE)
        #' df$set_key("mpg", "cyl")
        #' df$set_key(mpg, vs)
        set_key = function(...) {#browser()
            key <- stringify_dots(...)
            data.table::setkeyv(x=private$tbl, cols=key)
            invisible(self)
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

        #' @description Drop columns in place.
        #'
        #' @param ... Column names to remove. Can be quoted (as strings) or unquoted.
        #'
        #' @examples
        #' x <- DF(data.frame(a=1:5, b=1:5))
        #' x$drop("b")
        #' x$drop(a)
        drop = function(...) {
            if (missing(...)) return(invisible(self))
            cols <- stringify_dots(...)
            if (any(cols %in% private$find_group_cols(private$call$grouping()))) {
                stop("Can not drop columns used in grouping!")
            }
            private$tbl[, (c(cols)) := NULL]
            invisible(self)
        },

        #' @description Set the order of columns in place.
        #'
        #' @param order Vector of column names or column numbers specifying the order. See details.
        #'
        #' @details
        #' If length(order) < length(x),
        #' the specified columns are moved in order to the "front" of x.
        #' By default, reorder without a specified order moves the key columns in order to the "front".
        #'
        #' @examples
        #' x <- DF(data.frame(a=1:5, b=1:5))
        #' x$set_order(c("b", "a"))$print() # same as x$set_order("b")
        set_order = function(order="key") {
            if (identical(order, "key")) order <- self$key
            data.table::setcolorder(private$tbl, neworder = order)
            invisible(self)
        },

        #' @description Rename column names in place.
        #'
        #' @param mapping Named character vector.
        #' Names of the vector elements are the old names and the elements itself are the new names.
        #'
        #' @examples
        #' x <- DF(data.frame(a=1:5, b=1:5))
        #' x$rename(c("a"="A", "b"="B"))
        rename = function(mapping) {
            if (!is.character(mapping) || is.null(names(mapping))) stop("Provide a named character vector!")
            private$call$set(by = private$rename_grouping(private$call$grouping(), names(mapping), mapping))
            data.table::setnames(private$tbl, old=names(mapping), new=mapping)
            invisible(self)
        },

        #' @description Rename column names in place using a mapping function.
        #'
        #' @param mapper Function that accepts old column names as a character vector and returns a character vector of new column names.
        #'
        #' @examples
        #' x <- DF(data.frame(a=1:5, b=1:5))
        #' x$rename_with(toupper)
        #' custom_mapper = function(x) {return(paste0(x, 1))}
        #' x$rename_with(custom_mapper)
        rename_with = function(mapper) {
            if (!is.function(mapper)) stop("Provide a function that maps old names to new names!")
            cols <- names(private$tbl)
            private$call$set(by = private$rename_grouping(private$call$grouping(), cols, mapper(cols)))
            data.table::setnames(private$tbl, old=mapper)
            invisible(self)
        }

    ),

    active = list(

        #' @field columns Column names.
        columns = function() {
            names(private$tbl)
        },

        #' @field key Key getter.
        key = function() {
            data.table::key(private$tbl)
        }
    ),

    private = list(

        call = NULL,


        tbl = NULL,

        upd_join_add_missing_insert_names = function(e) {
            result <- if (!is.null(names(e))) names(e) else vector("character", length=length(e))
            missing <- which(result == "")[-1L]
            if (any(missing)) {
                for (i in missing) {
                    result[i] <- as.character(e[[i]])
                    e[[i]] <- as.name(paste0("x.", e[[i]]))
                }
                names(e) <- result
            }
            e
        },

        upd_join_rename_duplicated_cols = function(new_cols) {
            while (any(dupl <- new_cols %in% names(private$tbl))) {
                new_cols[dupl] <- paste0(new_cols[dupl], "_y")
            }
            new_cols
        },

        upd_join_error_handler = function(e) {
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
        },

        l_join_rename = function(result, on_x, on_y, names_y) {
            # x[y, on...]
            # if on column names differ, the column from y is not returned
            # duplicated columns in y are prefixed with i.
            # if y has a column of the same name of one of the on columns for x
            #(but is not used in the join), it is also added and prefixed with i.
            new_x <- old_x <- setdiff(names(private$tbl), on_x)
            new_y <- old_y <- setdiff(names_y, on_y)
            dupl <- new_x %in% names_y
            dupl_y <- old_y %in% c(old_x[dupl], on_x)
            old_x[dupl] <- paste0("i.", old_x[dupl])
            new_y[dupl_y] <- paste0(old_y[dupl_y], "_y")

            data.table::setnames(
                result,
                c(on_y, old_y, old_x),
                c(on_x, new_y, new_x)
            )
        },

        finalize_aggregate = function(result, f, by) {
            if (f[[1]] == quote(list)) {
                f <- f[-1L]
                f_names <- if (!is.null(names(f))) names(f) else vector("character", length=length(f))
                missing <- which(f_names=="")
                if (length(missing) > 0) {
                    for (i in missing) f_names[i] <- deparse(f[[i]][[1]])
                }
            } else {f_names <- deparse(f[[1]])}
            result[, fun := rep(f_names, times=dim(result)[1L] %/% length(f_names))]

            groups <- private$aggregate_add_names_by(by)
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
                i <- substitute(.__private__$tbl[, .I[tmp], by=BY]$V1)
            }
            call$set(i=i, env=parent.frame(2L))$eval(parent.frame(2L))
        },

        print_header = function() {
            d <- dim(private$tbl)
            title <- paste("Wrapping a", d[1], "x", d[2], "data.table.\n")
            group <- paste1("  Grouped by: ", private$call$grouping(TRUE), "\n")
            i <- paste1("  Using rows where:", private$call$arg("i", TRUE), "\n")
            sdcols <- paste1("  Columns subset using:", private$call$arg(".SDcols", TRUE), "\n")
            length <- max(sapply(c(title, group, i, sdcols), nchar)) - 1
            divisor <- paste0(rep("-", times = length), collapse="")
            cat(title, i, sdcols, group, divisor, "\n")
        },

        find_group_cols = function(e) {
            #CONSIDER moving to Call class?
            #REFACTOR
            if (is.null(e)) return(NULL)
            result <- character(length = length(e) - 1)
            xtr <- 0
            for (i in seq_along(e[-1L])) {
                el <- e[[i+1]]
                if (is.name(el)) {
                    result[i+xtr] <- as.character(el)
                    next
                }
                if (is.call(el)) {
                    multiple <- FALSE
                    for (j in seq_along(el)[-1L]) {
                        if (!is.name(el[[j]])) next
                        if (multiple) xtr <- xtr+1
                        result[i+xtr] <- as.character(el[[j]])
                        multiple <- TRUE
                    }
                }
            }
            result
        },

        rename_grouping = function(e, old, new) {
            if (is.null(e)) return()
            for (i in seq_along(e)[-1L]) {
                el <- e[[i]]
                if (is.name(el)) {
                    idx <- which(as.character(el)==old)
                    if (length(idx) == 1) e[[i]] <- as.name(new[idx])
                    next
                }
                if (is.call(el)) {
                    for (j in seq_along(el)[-1L]) {
                        if (!is.name(el[[j]])) next
                        idx <- which(as.character(el[[j]])==old)
                        if (length(idx)==1) e[[i]][[j]] <- as.name(new[idx])
                    }
                }
            }
            e
        }

    )
)
