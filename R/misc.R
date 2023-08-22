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
    # Problematic due to the dispatch. Printing the object in the console causes
    # loss of the actual parent/caller environment.
    x$print()
}

#' Check for any unnamed parameters in call.
#'
#' @param call A call (list) object.
#'
#' @return `TRUE` if atleast one parameter is not named.
any_unnamed <- function(call) {
    is.null(names(call)) || any(names(call)[-1L]=="")
}

#' Return ellipsis as character vector.
#'
#' @param ... Arbitrary values.
#'
#' @return A character vector with quoted arguments.
stringify_dots <- function(...) as.character(substitute(...()))


#' Merge two calls.
#'
#' @param c1 First call.
#' @param c2 Second call.
#'
#' @return A merged call using arguments from both c1 and c2.
merge_calls = function(c1, c2) {# calls may ybe null.
    if (is.null(c1) && is.null(c2)) return(NULL)
    if (!is.null(c1) && !is.null(c2) && c1[[1]] != c2[[1]]) {
        stop("Different calls passed.")
    }
    f <- ifelse(is.null(c1[[1]]), c2[[1]], c1[[1]])
    as.call(c(f, as.list(c1)[-1L], as.list(c2)[-1L]))
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
#'
#' @examples
#' mean_verbose <- extend_body(mean, quote(message("Calculating mean!")), 1L)
#' mean_verbose(1:5)
extend_body <- function(fun, with, position=1L) {
    b <- body(fun)
    if (is.null(b)) stop("Function has no body!")

    if (length(b) == 1 || b[[1]] != quote(`{`)) b <- call("{", b)
    nb <- as.call(append(as.list(b), with, after=position))

    body(fun) <- nb
    fun
}

paste1 <- extend_body(paste0, quote(if (any(sapply(list(...), is.null))) return(NULL)), 1L)


with_debugger <- function(f) {
    extend_body(f, quote(browser()), 1L)
}


