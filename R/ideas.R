#' # FILTER BY GROUP
#' #filter(max(mpg))
#' # if by is defined then filters by group
#' #my idea:mtcars[, .SD[mpg==max(mpg) & wt > 2], by = cyl]
#' #https://stackoverflow.com/questions/16573995/subset-rows-corresponding-to-max-value-by-group-using-data-table
#' # suggested way is faster, see bencharks:
#' # mtcars[mtcars[, .I[mpg==max(mpg)], by=cyl][, V1]]
#'
#' # if private$by is set then filter works on groups, setting private$i to private$tbl[, .I[mpg==max(mpg)], by=cyl]$V1
#'
#'
#' #' @description Summary on missing values.
#' #'
#' #'
#' #' @examples
#' #' sf <- SF(data.frame(a=c(1:5,NA, NA), b=c(1:6, NA)))
#' #' sf$missing_data_summary()
#' missing_data_summary = function() {
#'     call <- substitute(private$df$unwrap()[, lapply(.SD, function(x) list(sum(is.na(x)),
#'                                                                           round(mean(is.na(x))*100, 2))), .SDcols=columns])
#'     call[[".SDcols"]] <- substitute(columns)
#'     result <- eval(call)
#'     result <- as.data.table(t(result), keep.rownames = TRUE)
#'     setnames(result, c("column", "n_miss", "pct_miss"))
#'     result
#' }
#'
#' #' @description Apply a function to columns
#' #'
#' #' @param fun Function to apply. See details.
#' #' @param columns Optional list of columns to apply the function on. Can also be a predicate (e.g. `is.numeric`)
#' #' @param ... Additional arguments passed to `fun`
#' #'
#' #' @details
#' #' IMPORTANT: In order to use locally defined functions, they first have to be imported into the object with the `$import_function` method.
#' #' This ensures that they are found in the execution path.
#' #' @return A `StatFrame`.
#' #'
#' #' @examples
#' #' sf <- SF(data.frame(a=1:5, b=6:10))
#' #' add_2 <- function(x) x+2
#' #' # sf$import_function(add_2) # necessary only if sum_squares is defined locally
#' #' sf$map(add_2) #mean and sd are always globally accessible
#' #' sf$map(as.character)
#' map = function(fun, columns=NULL, ...) {
#'     call <- substitute(private$df$unwrap()[, lapply(.SD, fun, ...), .SDcols = cols])
#'     call[[".SDcols"]] <- private$parse_sdcols(substitute(columns))
#'     SF(eval(call))
#' }
#'
#' #' @description IN DEVELOPMENT. Show summary of the data
#' #'
#' #' @param col_type Optional type of columns to show the summary for. Defaults to all types.
#' #'
#' #' @return A `data.table`.
#' #'
#' #' @examples
#' #' sf <- SF(data.frame(a=1:5, b=6:10))
#' #' sf$describe()
#' describe = function(col_type=NULL) {browser()
#'     result = list(numeric=NULL, logical=NULL, factor=NULL, character=NULL)
#'     self$import_function(function(x) {mean(is.na(x))}, "prop_missing")
#'
#'     result$numeric <- self$aggregate(funs = list(min(x, na.rm=T), median(x, na.rm=T),
#'                                                  mean(x, na.rm=T), max(x, na.rm=T), prop_missing(x)),
#'                                      columns = is.numeric)
#'
#'     fct_cols <- names(private$df$unwrap())[sapply(private$df$unwrap(), is.factor)]
#'     if (length(fct_cols) > 0) {
#'         res_fct <- vector("list", length = length(fct_cols))
#'         for (i in seq_along(fct_cols)) res_fct[[i]] <- private$df$unwrap()[, .N, by = c(fct_cols[i])]
#'         result$factor <- res_fct
#'     }
#'
#'     result$character <- self$aggregate(funs = list(prop_missing(x)),
#'                                        columns = is.character)
#'
#'     result
#'
#' }
#
#
#
#
# safely <- function(f) {
#
# }
