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

        join = NULL,

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
