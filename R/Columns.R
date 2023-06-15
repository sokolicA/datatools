#' @title DataFrame Columns Class
#'
#' @description A class for column names of data objects of class `DataFrame`.
#'
#' @import data.table
Columns <- R6::R6Class(
    #CONSIDER merging the class into the main class.
    "Columns",

    public = list(
        #' @description Constructor
        #'
        #' @param env Private environment of the passed `DataFrame`.
        #'
        initialize = function(env=private) {
            private$df_env <- env
        },

        #' @description Print method
        #'
        #'
        print = function() {
            cat("Number of columns:", dim(private$df_env$tbl)[2])
            cat("\nColumn names: ")
            cat(names(private$df_env$tbl), sep = ", ")
        },

        #' @description Drop columns in place
        #'
        #' @param columns Character vector of the column names to remove.
        #'
        #' @examples
        #' x <- DF(data.frame(a=1:5, b=1:5))
        #' x$columns$drop("b") #
        #' x$columns$names
        drop = function(columns) {
            if (!is.character(columns)) stop("Provide a vector of column names!")
            if (any(columns %in% private$find_group_cols(private$df_env$call$grouping()))) stop("Can not drop columns used in grouping!")
            private$df_env$tbl[, (c(columns)) := NULL]
        },

        #' @description Reorder columns in place
        #'
        #' @param order Character vector of the new column name ordering.
        #' May also be column numbers. If length(order) < length(x),
        #' the specified columns are moved in order to the "front" of x.
        #' By default, reorder without a specified order moves the key columns in order to the "front".
        #'
        #' @examples
        #' x <- DF(data.frame(a=1:5, b=1:5))
        #' x$columns$reorder(c("b", "a")) # same as x_cols$reorder("b")
        #' x
        reorder = function(order=key(private$df_env$tbl)) {
            data.table::setcolorder(private$df_env$tbl, neworder = order)
            return(invisible(self))
        },

        #' @description Rename column names in place.
        #'
        #' @param mapping Named character vector. Names of the vector elements are the old names and the elements itself are the new names.
        #'
        #' @examples
        #' x <- DF(data.frame(a=1:5, b=1:5))
        #' x$columns$rename(c("a"="A", "b"="B"))
        rename = function(mapping) {
            if (!is.character(mapping) || is.null(names(mapping))) stop("Provide a named character vector!")
            private$df_env$call$set(by = private$rename_grouping(private$df_env$call$grouping(), names(mapping), mapping))
            data.table::setnames(private$df_env$tbl, old=names(mapping), new=mapping)
            return(invisible(self))
        },

        #' @description Rename column names in place using a mapping function.
        #'
        #' @param mapper Function that accepts old column names as a character vector and returns a character vector of new column names.
        #'
        #' @examples
        #' x <- DF(data.frame(a=1:5, b=1:5))
        #' x$columns$rename_with(toupper)
        #' custom_mapper = function(x) {return(paste0(x, 1))}
        #' x$columns$rename_with(custom_mapper)
        rename_with = function(mapper) {
            if (!is.function(mapper)) stop("Provide a function that maps old names to new names!")
            private$df_env$call$set(by = private$rename_grouping(private$df_env$call$grouping(), self$names, mapper(self$names)))
            data.table::setnames(private$df_env$tbl, old=mapper)
            return(invisible(self))
        }

    ),

    active = list(
        #' @field names Vector of column names.
        names = function() names(private$df_env$tbl)
    ),

    private = list(
        df = NULL,

        df_env = NULL,

        find_group_cols = function(e) {
            if (is.null(e)) return()
            result <- character(length = length(e) - 1)
            xtr <- 0
            for (i in seq_along(e[-1L])) {
                el <- e[[i+1]]
                if (is.name(el)) {
                    result[i+xtr] <- as.character(el)
                    next
                }
                if (is.call(el)) {
                    multiple <- FALSE
                    for (j in seq_along(el)[-1L]) {
                        if (!is.name(el[[j]])) next
                        if (multiple) xtr <- xtr+1
                        result[i+xtr] <- as.character(el[[j]])
                        multiple <- TRUE
                    }
                }
            }
            result
        },

        rename_grouping = function(e, old, new) {
            if (is.null(e)) return()
            for (i in seq_along(e)[-1L]) {
                el <- e[[i]]
                if (is.name(el)) {
                    idx <- which(as.character(el)==old)
                    if (length(idx) == 1) e[[i]] <- as.name(new[idx])
                    next
                }
                if (is.call(el)) {
                    for (j in seq_along(el)[-1L]) {
                        if (!is.name(el[[j]])) next
                        idx <- which(as.character(el[[j]])==old)
                        if (length(idx)==1) e[[i]][[j]] <- as.name(new[idx])
                    }
                }
            }
            e
        }
    )
)
