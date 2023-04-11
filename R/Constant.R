#' @title Constant Object Class
#'
#' @description Objects of this class are holders of constant values.
#' Constants defined inside a `Constant` object can not be modified.
#' The values are stored inside a `list` object. They should be accessed with the `$get` method.
#' See examples for a simple use case.
#'
#' @examples
#' CONST <- Const() # or Constant$new()
#' CONST$define("x", 3)
#' CONST$get$x
#'
#' CONST$define("y", c("a", "b"))
#' CONST$define("y", 2) # Error
#'
#' @import data.table
#' @import R6
Constant <- R6::R6Class(
    "Constant",
    cloneable = FALSE,
    lock_class = TRUE,

    public = list(
        #' @description Define a new constant.
        #'
        #' @param name Character Name of the constant.
        #' @param value Value assigned to the constant.
        #'
        #' @return Nothing.
        define = function(name, value) {
            private$abort_if_already_exists(name)
            private$abort_if_not_legal(name)
            private$values[[name]] <- data.table::copy(value)
        },

        #' @description Print function.
        #'
        #' @return Nothing.
        print = function() {
            print(private$values)
        }
    ),

    active = list(
        #' @field get Access the defined constants.
        get = function() data.table::copy(private$values)
    ),

    private = list(

        values = list(),

        abort_if_already_exists = function(name) {
            if (!is.null(private$values[[name]])) {
                stop("Constant with this name is already defined!")
            }
        },

        abort_if_not_legal = function(name) {
            if (!is.character(name)) stop("Only character names are allowed!")
            if (length(name) != 1) stop("Provide exactly one name!")
            if (name == "") stop("Names must be non empty!")
            if (grepl(" ", name)) stop("Spaces in name are not allowed! Use either snake case, camelCase or PascalCase.")
        }
    )
)
