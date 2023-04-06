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

        is_key_unique = function() {
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
        count = function(by=.()) {
            call <- quote(private$.tbl[, .N, keyby = by])
            call[[5]] <- substitute(by)
            eval(call)
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

        #' @description Create a new `DataFrame` by appending tables using column names.
        #'
        #' @param ... Objects of class `data.frame`.
        #' @param fill Optional parameter whether to fill missing columns with `NA`. Defaults to `FALSE`.
        #'
        #' @return A new unkeyed `DataFrame` object with rows appended.
        #'
        #' @examples
        append = function(..., fill=FALSE) {
            result <- rbindlist(
                list(private$.tbl, ...),
                use.names = TRUE,
                fill = fill
            )
            return(DF(result, key=NULL))
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

        #' @description Update table columns by reference.
        #'
        #' @param columns A list of columns to update or add and the corresponding calculation.
        #' @param where Optional expression/integer vector/logical vector specifying which rows to update. Defaults to all rows.
        #'
        #' @return Nothing.
        #'
        #' @examples
        #'    df <- DF(data.table(a=1:5, b=1:5))
        #'    df$update(.(a = 2), b == 3)
        #'    df$update(list(g = a, dd = ifelse(a==2, b, 0)), 1:2)
        update = function(columns, where=NULL) {
            call <- quote(private$.tbl[, j])
            condition <- substitute(where)
            if (!is.null(condition)) call[[3]] <-condition
            j_sub <- substitute(columns)
            j_sub[[1]] <- quote(`:=`)
            call[[4]] <- j_sub
            eval(call)
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

        #' @description Drop columns in place
        #'
        #' @param column Character vector of the column names to remove.
        #'
        #' @examples
        #' x <- data.table(a=1:5, b=1:5)
        #' x_cols <- Columns$new(x)
        #' x_cols$drop("b") #
        #' names(x)
        drop = function(columns) {
            if (!is.character(columns)) stop("Provide a vector of column names!")
            private$.tbl[, (c(columns)) := NULL]
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
