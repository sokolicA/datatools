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


merge_calls = function(c1, c2) {
    as.call(c(as.list(c1), as.list(c2)[-1L]))
}
