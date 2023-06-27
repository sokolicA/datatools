#' Convert `DataFrame` to `data.frame`
#'
#' @param x A `DataFrame` object.
#' @param row.names Not used.
#' @param optional Not used.
#' @param ... Not used.
#'
#' @return A `data.frame` object.
#' @export
as.data.frame.DataFrame <- function(x, row.names=NULL, optional=NULL,...) {
    data.frame(x$unwrap())
}


#' Convert `DataFrame` to `data.table`
#'
#' @param x A `DataFrame` object.
#'
#' @return A `data.table` object.
#' @export
as.data.table.DataFrame <- function(x) {
    x$unwrap()
}


#' S3 print method of `DataFrame`
#'
#' @param x A `DataFrame` object.
#' @param ... Not used.
#'
#' @return Invisibly returns the `DataFrame`.
#' @export
print.DataFrame <- function(x, ...) {
    eval(x$print(), envir=parent.frame(2L))
}



merge_calls = function(c1, c2) {
    as.call(c(as.list(c1), as.list(c2)[-1L]))
}


find_obj_env <- function(name, start=parent.frame()) {
    stopifnot(is_string(name))

    if (identical(start, emptyenv())) {
        stop("Can't find ", name, call. = FALSE)
    }

    if (exists(name, start, inherits = FALSE)) {
        start
    } else {
        find_obj_env(name, parent.env(start))
    }
}



#' Extend the body of a function.
#'
#' @param fun The function of which the body is to be extended.
#' @param with A language object to be inserted into the function body of `fun`.
#' @param position The position where `with` is to be inserted. Either 1 (before) or 2 (after) the existing body.
#'
#' @details A function must have a body in order to be able to extend it.
#' Primitive functions do not have a function body and trying to extend their body will result in an error.
#' See chapter 6.2.2 Primitive functions in https://adv-r.hadley.nz/functions.html.
#'
#'
#' @return A new function with the extended body.
#' @export
#'
#' @examples
#' mean_verbose <- extend_body(mean, quote(message("Calculating mean!")), 1L)
#' mean_verbose(1:5)
extend_body <- function(fun, with, position=1L) {
    b <- body(fun)
    if (is.null(b)) stop("Function has no body!")

    if (length(b) == 1 || b[[1]] != quote(`{`)) b <- call("{", b)
    nb <- as.call(append(as.list(b), with, after=position))

    body(f) <- nb
    f
}

paste1 <- extend_body(paste0, quote(if (any(sapply(list(...), is.null))) return(NULL)), 1L)
