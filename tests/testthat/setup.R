
preprocess_ellipsis <- function(...) substitute(list(...))

#' @title Expose private members of a R6 Class
#'
#' @description Useful for testing the private behaviour.
#'
#' @param class An R6 Class Generator object.
#' @param methods Optional character vector of methods to expose. Defaults to all private methods.
#' @param fields Optional character vector of methods to expose. Defaults to all private fields.
#' @param debug Optional boolean specifying whether to insert a debugger command into the methods.
#'
#' @return A R6 Class Generator object with exposed methods.
#'
#' @details Names of exposed private members start with `private_`.
#' Private methods are copied as public methods.
#' Private fields are exposed as active bindings of the actual private fields.
#'
expose_R6_private <- function(class, methods=NULL, fields=NULL, debug=FALSE) {
    if (missing(class) || class(Call) != "R6ClassGenerator") {
        stop("Must provide R6 class generator object!")
    }
    if (is.null(methods)) methods <- names(class$private_methods)
    if (is.null(fields)) fields <- names(class$private_fields)

    result <- R6::R6Class(paste0("Test", class$classname), inherit=class)

    if (is.character(methods)) {
        for (method in methods) {
            m <- class$private_methods[[method]]
            if (is.null(m)) next;
            if (debug) m <- with_debugger(m)
            result$set("public", paste0("private_", method), value=m, overwrite = TRUE)
        }
    }

    if (is.character(fields)) {
        for (field in fields) {
            if (!is.element(field, names(class$private_fields))) next;
            f <- function() {}
            functionBody(f) <- str2lang(paste0("private$", field))
            result$set("active", paste0("private_", field), value=f, overwrite = TRUE)
        }
    }

    result
}


#
# f1 <- mean
# f2 <- is.integer # error
# f3 <- function(x) x
# f4 <- function(x) {x}


