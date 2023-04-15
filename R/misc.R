#' Convert `DataFrame` to `data.frame`
#'
#' @param df A `DataFrame` object.
#'
#' @return A `data.frame` object.
#' @export
as.data.frame.DataFrame <- function(df) {
    data.frame(df$unwrap())
}


#' Convert `DataFrame` to `data.table`
#'
#' @param df A `DataFrame` object.
#'
#' @return A `data.table` object.
#' @export
as.data.table.DataFrame <- function(df) {
    df$unwrap()
}
