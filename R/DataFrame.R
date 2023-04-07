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
        #' @description `DataFrame` Constructor
        #'
        #' @param tbl An object of class `data.frame`.
        #' @param key Optional vector of column names. Setting a key sorts the table in RAM using the values of the key column(s). See Details.
        #' @param id Optional ID of the object. Not currently used.
        #'
        initialize = function(tbl, key = NULL, id = NULL) {
            data.table::setDT(tbl, key = key)
            private$.tbl <- tbl
            private$.id <- id
        },

        #' @description Print the table object.
        #'
        #' @details The method used is `print.data.table`.
        #'
        print = function() {
            print(private$.tbl)
        },

        #' @description Sort the table rows
        #'
        #' @param ...  The columns to sort by. Do not quote column names. See `?data.table::setorder`.
        #'
        #' @return Invisibly returns itself.
        #'
        #' @examples
        #'    df <- DF(data.table(a=1:5, b=1:5))
        #'    df$sort(-b)
        sort = function(...) {
            if (!is.null(self$key)) stop("Table is already sorted with key!")
            data.table::setorder(private$.tbl, ...)
            return(invisible(self))
        },

        #' @description Check whether keys represent unique entries
        #'
        #' @return `TRUE` if the set key is unique and `FALSE` otherwise. If no key is set the result is `FALSE`.
        #'
        #' @examples
        #' df <- DataFrame$new(data.table(a=1:5, b=1:5), key = c("a"))
        #' df$is_key_unique()
        #' df <- DataFrame$new(data.table(a=1:5, b=1:5))
        #' df$is_key_unique()
        is_key_unique = function() {
            if (is.null(self$key)) return(FALSE)
            uniqueN(private$.tbl, by = self$key) == nrow(private$.tbl)
        },

        #' @description Count the number of rows.
        #'
        #' @param by An optional list() specifying the columns to group by. Defaults to no grouping.
        #'
        #' @return A data.table with the row counts in the `N` column.
        #'
        #' @examples
        #' df <- DataFrame$new(data.table(a=1:5, b=1:5))
        #' df$count()
        #' df <- DataFrame$new(data.table(a=c(1,1,1,2,3), b=1:5))
        #' df$count(by = list(a)) # df$count(by = .(a))
        count = function(by=NULL) {
            eval(substitute(private$.tbl[, .N, keyby = by]))
        },

        #' @description Update table columns by reference.
        #'
        #' @param columns A list of columns to update or add and the corresponding calculation.
        #' @param where Optional expression/integer vector/logical vector specifying which rows to update. Defaults to all rows.
        #' @param by Optional list of columns to group by before performing the update. Defaults to no grouping.
        #'
        #' @return Nothing.
        #'
        #' @examples
        #'    df <- DF(data.table(a=1:5, b=1:5))
        #'    df$update(.(a = 2), b == 3)
        #'    df$update(list(g = a, dd = ifelse(a==2, b, 0)), 1:2)
        update = function(columns, where=NULL, by=NULL) {
            call <- substitute(private$.tbl[, j, by = by])
            condition <- substitute(where)
            if (!is.null(condition)) call[[3]] <-condition
            j_sub <- substitute(columns)
            j_sub[[1]] <- quote(`:=`)
            call[[4]] <- j_sub
            eval(call)
        },

        #' @description Perform an update join.
        #'
        #' Add new or update current table rows on matching positions.
        #'
        #' @param relationship A `Relationship` object with `right` table and `on` specified. See details.
        #' @param columns List of column names to update and/or add. Defaults to `NULL` which adds all columns. Can also be transformations of columns. See details.
        #' @param where Optional expression/integer vector/logical vector specifying which rows to update. Defaults to all rows.
        #'
        #' @return Returns the updated itself. Returns an error if there are multiple matches found in the `right` table.
        #'
        #' @details
        #' A `Relationship` object with a specified `right` table and `on` condition.
        #' The `left` table of the relationship will be set to the current object's table.
        #' This will override an existing `left` specification.
        #' The `right` table is joined to the `left` (current) table based on the `on` condition.
        #' Multiple Conditions provided in `on` have to be separated by commas. Also note that `on(x)` is the same as `on(x=x)`.
        #'
        #' Using columns from the `left` (current) table in calculations provided in `add` can be done by
        #' prefixing the column names with **`i.`**. See examples.
        #'
        #' Note that `list(...)` can be aliased with `.(...)` due to the background use of `data.table`.
        #'
        #' The join will not be performed if there are multiple matches found in the `right` table.
        #' In this case either use `left_join` or delete duplicated foreign keys in the `right` table.
        #'
        #'
        #' @examples
        #' df <- DF(data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3))
        #' y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])
        #' rel <- Rel(right=y)$on(x = y) # same as Relationship$new(right=y)$on(x = y)
        #' df$update_join(rel, columns=list(a=3, c=ifelse(i.x == 1, 3, 2), z)) #i.x is from the table stored in df
        #' df$update_join(rel, columns=list(g=c**2), where=x %in% 1:2)
        update_join = function(relationship, columns=NULL, where=NULL) {
            relationship$left <- private$.tbl
            join <- UpdateJoin$new(relationship)
            join$add_sub(columns=substitute(columns), where=substitute(where))
            return(invisible(self))
        },

        #' @description Transform columns with function
        #'
        #' @param columns Either a list of columns to transform or a function that evaluates to boolean with the column names as argument. See details.
        #' @param fun A function that is applied to the selected columns.
        #' @param where Optional expression/integer vector/logical vector specifying which rows to update. Defaults to all rows.
        #' @param ... Optional arguments that are passed to `fun`.
        #'
        #' @details If a function is passed as argument to `columns` it must be prefixed with en exclamation mark (**`!`**) and the function's argument
        #' that takes the column names must be aliased with **`.names`**. See examples.
        #'
        #' @return Nothing.
        #'
        #' @examples
        #'    df <- DF(data.table(a=1:5, b=1:5))
        #'    df$transform(.(a, b), function(x) x*2, b%%2==0)
        #'    df$transform(! .names %in% c("a"), function(x) x*2, b>2)
        #'    df$transform(! grepl("^a", .names), function(x) x*2, a != 1 & a > b)
        transform = function(columns, fun, where=NULL, ...) {
            call <- quote(private$.tbl[, j, .SDcols=x])

            col_sub <- substitute(columns)
            col_names <- private$parse_colnames(col_sub)
            condition <- substitute(where)
            if (!is.null(condition)) call[[3]] <- condition
            j_sub <- quote(`:=` (x, y))
            col_names_sub <- str2lang(paste0("c(", paste0("'", col_names, "'", collapse = ","), ")"))
            j_sub[[2]] <- col_names_sub
            j_sub[[3]] <-  substitute(lapply(.SD, fun, ...))
            call[[4]] <- j_sub
            call[[5]] <- col_names_sub
            eval(call)
            return(self)
        },

        #' @description Transform across columns with function
        #'
        #' @param predicate A function that is applied to the columns.  The variables for which `predicate` returns TRUE are selected.
        #' @param fun A transformation function that is applied to the selected columns.
        #' @param where Optional expression/integer vector/logical vector specifying which rows to update. Defaults to all rows.
        #' @param ... Optional arguments that are passed to `fun`.
        #'
        #'
        #' @return Nothing.
        #'
        #' @examples
        #'     df <- DF(data.table(a=1:5, b=1:5, c = paste0("  ", 1:5, "   ")))
        #'     df$transform_if(is.numeric, function(x) x*2)
        #'     df$transform_if(is.character, trimws)
        transform_if = function(predicate, fun, where=NULL, ...) {
            if (!is.function(predicate)) stop("Must provide a function to determine which columns will be transformed!")

            call_predicate <- quote(sapply(private$.tbl, fun, simplify = TRUE))
            call_predicate[[3]] <- predicate
            col_names <- names(private$.tbl)[eval(call_predicate)]
            if(length(col_names) == 0) return(warning("No columns found using the predicate function!"))

            call <- quote(private$.tbl[, j, .SDcols=x])
            col_sub <- substitute(columns)
            condition <- substitute(where)
            if (!is.null(condition)) call[[3]] <- condition
            j_sub <- quote(`:=` (x, y))
            col_names_sub <- str2lang(paste0("c(", paste0("'", col_names, "'", collapse = ","), ")"))
            j_sub[[2]] <- col_names_sub
            j_sub[[3]] <-  substitute(lapply(.SD, fun, ...))
            call[[4]] <- j_sub
            call[[5]] <- col_names_sub
            eval(call)
            return(self)
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
        #' df <- DataFrame$new(data.table(a=1:5, b=1:5))
        #' df$remove(a > 2)
        #' df <- DataFrame$new(data.table(a=1:5, b=1:5))
        #' df$remove(c(1, 3, 5))
        #' df <- DataFrame$new(data.table(a=1:3, b=1:3))
        #' df$remove(c(TRUE, NA, FALSE))
        remove = function(where) {
            condition <- substitute(where)
            if (!is.language(condition)) {
                stop(err$remove$not_language)
            }

            remove_rows <- eval(condition, envir=private$.tbl, enclos=parent.frame())
            if (is.numeric(remove_rows)) {
                if (max(remove_rows) > private$.tbl[,.N]) stop(err$remove$int_out_of_bounds)
                if (any(duplicated(remove_rows))) stop(err$remove$int_duplicated)
                remove_rows <- private$.tbl[, .I %in% remove_rows]
                keep <- !remove_rows
            }
            if (is.logical(remove_rows)) {
                if(length(remove_rows) != private$.tbl[,.N]) stop(err$remove$unequal_length_logical)
                keep <- !remove_rows | is.na(remove_rows)
            }

            removed <- DataFrame$new(private$.tbl[remove_rows], key=self$key)
            private$.tbl <- private$.tbl[keep]
            return(removed)
        },

        #' @description Perform a left outer join
        #'
        #' @param relationship A `Relationship` object with `right` table and `on` specified. See details.
        #' @param add Optional list of column names to add. Defaults to `NULL` which adds all columns. Can also be transformations of columns. See details.
        #'
        #' @return A new (keyed) `DataFrame` object extended with the columns from the joined table.
        #'
        #' @details
        #' A `Relationship` object with a specified `right` table and `on` condition.
        #' The `left` table of the relationship will be set to the current object's table.
        #' This will override an existing `left` specification.
        #' The `right` table is joined to the `left` (current) table based on the `on` condition.
        #' Multiple Conditions provided in `on` have to be separated by commas. Also note that `on(x)` is the same as `on(x=x)`.
        #'
        #' Using columns from the `left` (current) table in calculations provided in `add` can be done by
        #' prefixing the column names with `i.`. See examples.
        #'
        #'  Note that `list(...)` can be aliased with `.(...)` due to the background use of `data.table`.
        #'
        #' @examples
        #' df <- DF(data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3))
        #' y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])
        #' rel <- Rel(right=y)$on(x = y) # same as Relationship$new(right=y)$on(x = y)
        #' df$left_join(rel, add=.(a=3, c=ifelse(i.x==1, 3, 2), z, d=x)) #i.x is from the table stored in df
        left_join = function(relationship, add=NULL) {
            relationship$left <- private$.tbl
            join <- LeftJoin$new(relationship)
            result <- join$add_sub(substitute(add))
            return(DF(result, key = self$key))
        },

        #' @description Create a new `DataFrame` by appending tables using column names.
        #'
        #' @param ... Objects of class `data.frame`.
        #' @param fill Optional parameter whether to fill missing columns with `NA`. Defaults to `FALSE`.
        #'
        #' @return A new unkeyed `DataFrame` object with rows appended.
        #'
        #' @examples
        #' x <- data.table(a=1:5, b=1:5)
        #' y <- data.table(a=1:5, b=1:5)
        #' df <- DataFrame$new(x)
        #' res <- df$append(y, x, y)
        append = function(..., fill=FALSE) {
            result <- rbindlist(
                list(private$.tbl, ...),
                use.names = TRUE,
                fill = fill
            )
            return(DF(result, key=NULL))
        },

        #' @description Create a deep copy
        #'
        #' @return A copy of the `DataFrame`
        #'
        copy = function() {
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

        join = NULL,

        parse_colnames = function(e) {
            if (e[[1]] == substitute(`!`)) {
                name_func <- e[[2]]
                if (name_func[[1]] == substitute(`!`)) {
                    for (i in seq_along(name_func)[-1L])
                        if (name_func[[-1L]][[i]] == substitute(.names)) {
                            name_func[[-1L]][[i]] <- quote(names(private$.tbl))
                            break;
                        }
                } else {
                    for (i in seq_along(name_func)[-1L])
                        if (name_func[[i]] == substitute(.names)) {
                            name_func[[i]] <- quote(names(private$.tbl))
                            break;
                        }
                }

                result <- names(private$.tbl)[eval(name_func)]
            } else {result <- as.character(e)[-1L]}
            result
        },

        err = list(
            remove = list(
                not_language = "Provide either an expression, an integer vector specifying rows or a logical vector of same length as the number of rows!",
                unequal_length_logical = "Provided logical vector does not match the number of rows!",
                int_out_of_bounds = "Rows specified are out of bounds!",
                duplicated_int = "Duplicated row numbers not allowed!"
            )
        )
    )
)
