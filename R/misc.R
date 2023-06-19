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
