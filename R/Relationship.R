#' @title Relationship Class
#'
#' @description A class for relationships between objects of class `data.frame`.
#'
#' @import data.table
#'
Relationship <- R6::R6Class(
    "Relationship",
    public = list(
        #' @description Relationship constructor
        #'
        #' @param left Optional object of class `data.table` (or `data.frame`, see Details) that is treated as the left table.
        #' @param right Optional object of class `data.table` (or `data.frame`, see Details) that is treated as the right table.
        initialize = function(left=NULL, right=NULL) {
            if(!is.null(left)) self$left <- left;
            if(!is.null(right)) self$right <- right;
        },

        #' @description Get the relationship connection.
        #'
        #' @return The connection.
        #'
        get_on = function() {
            private$.on
        },

        #' @description Specify the columns the relationship between left and right is based on.
        #'
        #' @param ... Which columns in the left table relate to which columns of the right table.
        #'
        #' @return Invisibly returns itself.
        #'
        on = function(...) {
            private$.on <- substitute(list(...))
            return(invisible(self))
        },

        #' @description Check if the relationship is not fully specified.
        #'
        #' @return `TRUE` if `left`, `right` and `on` are specified and `FALSE` otherwise.
        #'
        is_not_fully_specified = function() {
            return(any(is.null(private$.left), is.null(private$.right), is.null(private$.on)))
        }
    ),

    active = list(
        #' @field left The left table.
        left = function(x) {
            if (!missing(x)) {
                if (!is.data.table(x)) x <- as.data.table(x)
                private$.left <- x
            } else {
                private$.left
            }
        },

        #' @field right The right table.
        right = function(x) {
            if (!missing(x)) {
                if (!is.data.table(x)) x <- as.data.table(x)
                private$.right <- x
            } else {
                private$.right
            }
        }
    ),

    private = list(
        .left = NULL,
        .right = NULL,
        .on = NULL
    )
)
