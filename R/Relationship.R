#' @title Relationship Class
#'
#' @description A class for relationships between objects of class `data.frame`.
#'
#' @import data.table
#'
Relationship <- R6::R6Class(
    "Relationship",
    public = list(
        initialize = function(left=NULL, right=NULL) {
            if(!is.null(left)) self$left <- left;
            if(!is.null(right)) self$right <- right;
        },

        get_on = function() {
            private$.on
        },

        on = function(...) {
            private$.on <- substitute(list(...))
            return(invisible(self))
        },

        is_not_fully_specified = function() {
            return(any(is.null(private$.left), is.null(private$.right), is.null(private$.on)))
        }
    ),

    active = list(
        left = function(x) {
            if (!missing(x)) {
                if (!is.data.table(x)) x <- as.data.table(x)
                private$.left <- x
            } else {
                private$.left
            }
        },

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
