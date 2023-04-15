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
#' df <- DF(data.table(a=1, b=2), key="a")
#'
#' @return A `DataFrame` object.
#'
#' @export
DF <- function(tbl, key = NULL, id = NULL) {
    DataFrame$new(tbl, key, id)
}


#' StatFrame constructor sugar
#' @describeIn StatFrame StatFrame$new() constructor sugar
#' @param df An object of class `DataFrame` or `data.frame`.
#'
#'
#' @examples
#' sf <- SF(data.table(a=1, b=2, key="a"))
#' sf <- SF(DF(data.table(a=1, b=2), key="a"))
#'
#' @return A `StatFrame` object.
#'
#' @export
SF <- function(df) {
    StatFrame$new(df)
}

#' Relationship constructor sugar
#' @describeIn Relationship Relationship$new() constructor sugar

#' @param left Optional object of class `data.table` (or `data.frame`, see Details) that is treated as the left table.
#' @param right Optional object of class `data.table` (or `data.frame`, see Details) that is treated as the right table.
#'
#' @details If the objects passed are not `data.table` objects then a `data.table` copy of the objects will be created with `data.table::as.data.table`.
#'
#'
#' @return A `Relationship` object.
#'
#' @export
Rel <- function(left=NULL, right=NULL) {
    Relationship$new(left, right)
}

#' Constant constructor sugar
#' @describeIn Constant Constant$new() constructor sugar
#'
#' @return A `Constant` object.
#'
#' @export
Const <- function() {
    Constant$new()
}


