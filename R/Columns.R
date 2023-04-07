#' @title DataFrame Columns Class
#'
#' @description A class for column names of data objects of class `data.frame`.
#'
#' @import data.table
Columns <- R6::R6Class(
    "Columns",

    public = list(
        #' @description Constructor
        #'
        #' @param x A `data.table` object.
        #'
        initialize = function(x) {
            private$.tbl <- x
        },

        #' @description Print method
        #'
        #'
        print = function() {
            cat("Number of columns:", dim(private$.tbl)[2])
            cat("\nColumn names: ")
            cat(names(private$.tbl), sep = ", ")
        },

        #' @description Drop columns in place
        #'
        #' @param columns Character vector of the column names to remove.
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
        #' @field names Vector of column names.
        names = function() names(private$.tbl)
    ),

    private = list(
        .tbl = NULL
    )
)
