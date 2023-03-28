#' DataFrame constructor sugar
#' @describeIn DataFrame DataFrame$new() constructor sugar
#' @param tbl An object of class `data.frame`.
#' @param key Optional vector of column names. Setting a key sorts the table in RAM using the values of the key column(s). See Details.
#' @param id Optional ID of the object. Not currently used.
#'
#' @details
#'
#' The key parameter is passed to `data.table::setkey(key)`. If the parameter is not passed, the existing keys of `x` (if any) will be kept.
#'
#' @examples
#'
#' @return A `DataFrame` object.
#'
#' @export
DF <- function(tbl, key = NULL, id = NULL) {
    DataFrame$new(tbl, key, id)
}
