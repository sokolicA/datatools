#' DataFrame constructor sugar
#' @describeIn DataFrame DataFrame$new() constructor sugar
#' @param tbl An object of class `data.frame`.
#' @param copy Optional argument specifying whether to wrap a copy of the passed object. Defaults to `FALSE`.
#'
#'
#' @examples
#' df <- DF(data.table(a=1, b=2))
#'
#' @return A `DataFrame` object.
#'
#' @export
DF <- function(tbl, copy=FALSE) {
    DataFrame$new(tbl, copy)
}


