#' DataFrame constructor sugar
#' @describeIn DataFrame DataFrame$new() constructor sugar
#' @param tbl An object of class `data.frame`.
#' @param key Optional vector of column names. Setting a key sorts the table in RAM using the values of the key column(s). See Details.
#'
#' @details
#'
#' The key parameter is passed to `data.table::setkey(key)`. If the parameter is not passed, the existing keys of `x` (if any) will be kept.
#'
#' @examples
#' df <- DF(data.table(a=1, b=2), key="a")
#'
#' @return A `DataFrame` object.
#'
#' @export
DF <- function(tbl, key = NULL) {
    DataFrame$new(tbl, key)
}


