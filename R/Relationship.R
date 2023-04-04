#' Relationship class
#'
#' @param left
#' @param right
#'
#' @describeIn Relationship Object
#'
#' @import data.table
#'
#' @examples
Relationship <- R6::R6Class(
    "Relationship",
    public = list(
        initialize = function(left, right) {
            if(!missing(left)) private$.left <- left;
            if(!missing(right)) private$.right <- right;
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
                private$.left <- x
            } else {
                private$.left
            }
        },

        right = function(x) {
            if (!missing(x)) {
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
