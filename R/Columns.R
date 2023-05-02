#' @title DataFrame Columns Class
#'
#' @description A class for column names of data objects of class `DataFrame`.
#'
#' @import data.table
Columns <- R6::R6Class(
    "Columns",

    public = list(
        #' @description Constructor
        #'
        #' @param x A `DataFrame` object.
        #'
        initialize = function(x) {
            private$df <- x
        },

        #' @description Print method
        #'
        #'
        print = function() {
            cat("Number of columns:", dim(private$df$unwrap())[2])
            cat("\nColumn names: ")
            cat(names(private$df$unwrap()), sep = ", ")
        },

        #' @description Drop columns in place
        #'
        #' @param columns Character vector of the column names to remove.
        #'
        #' @examples
        #' x <- DF(data.frame(a=1:5, b=1:5))
        #' x$columns$drop("b") #
        #' x$columns$names
        drop = function(columns) {
            if (!is.character(columns)) stop("Provide a vector of column names!")
            private$df$unwrap()[, (c(columns)) := NULL]
        },

        #' @description Reorder columns in place
        #'
        #' @param order Character vector of the new column name ordering.
        #' May also be column numbers. If length(order) < length(x),
        #' the specified columns are moved in order to the "front" of x.
        #' By default, reorder without a specified order moves the key columns in order to the "front".
        #'
        #' @examples
        #' x <- DF(data.frame(a=1:5, b=1:5))
        #' x$columns$reorder(c("b", "a")) # same as x_cols$reorder("b")
        #' x
        reorder = function(order=key(private$df$unwrap())) {
            data.table::setcolorder(private$df$unwrap(), neworder = order)
            return(invisible(self))
        },

        #' @description Rename column names in place.
        #'
        #' @param mapping Named character vector. Names of the vector elements are the old names and the elements itself are the new names.
        #'
        #' @examples
        #' x <- DF(data.frame(a=1:5, b=1:5))
        #' x$columns$rename(c("a"="A", "b"="B"))
        rename = function(mapping) {
            if (!is.character(mapping) || is.null(names(mapping))) stop("Provide a named character vector!")
            data.table::setnames(private$df$unwrap(), old=names(mapping), new=mapping)
            return(invisible(self))
        },

        #' @description Rename column names in place using a mapping function.
        #'
        #' @param mapper Function that accepts old column names as a character vector and returns a character vector of new column names.
        #'
        #' @examples
        #' x <- DF(data.frame(a=1:5, b=1:5))
        #' x$columns$rename_with(toupper)
        #' custom_mapper = function(x) {return(paste0(x, 1))}
        #' x$columns$rename_with(custom_mapper)
        rename_with = function(mapper) {
            if (!is.function(mapper)) stop("Provide a function that maps old names to new names!")
            data.table::setnames(private$df$unwrap(), old=mapper)
            return(invisible(self))
        }

    ),

    active = list(
        #' @field names Vector of column names.
        names = function() names(private$df$unwrap())
    ),

    private = list(
        df = NULL
    )
)
