StatFrame <- R6::R6Class(
    "StatFrame",
    public = list(
        initialize = function(x) {
            private$df <- x
        },

        count = function(by=NULL) {
            private$df$count(by)
        },

        #' Data aggregation
        #'
        #' @param funs A list of functions used to create an aggregate summary.
        #' @param columns Optional list of columns to aggregate or a predicate.
        #' @param where Filtering
        #' @param by By group.
        #'
        #' @return
        #'
        #' @examples
        aggregate = function(funs, columns=NULL, where=NULL, by=NULL) {browser()
            call <- substitute(private$df$data[, lapply(.SD, function(x) {funs}), .SDcols = cols, keyby=by])
            condition <- substitute(where)
            if (!is.null(condition)) call[[3]] <- condition
            call[[".SDcols"]] <- private$parse_sdcols(substitute(columns))

            result <- eval(call)
            private$finalize_aggregate(result, substitute(funs), substitute(by))
            result[]
        },

        sum = function(columns=is.numeric, where=NULL, by=NULL) {
            private$apply_expr(sum, substitute(columns), substitute(where), substitute(by), na.rm=T)
        },

        mean = function(columns=is.numeric, where=NULL, by=NULL) {
            private$apply_expr(mean, substitute(columns), substitute(where), substitute(by), na.rm=T)
        },

        median = function(columns=is.numeric, where=NULL, by=NULL) {
            private$apply_expr(median, substitute(columns), substitute(where), substitute(by), na.rm=T)
        },

        min = function(columns=is.numeric, where=NULL, by=NULL) {
            private$apply_expr(min, substitute(columns), substitute(where), substitute(by), na.rm=T)
        },

        max = function(columns=is.numeric, where=NULL, by=NULL) {
            private$apply_expr(max, substitute(columns), substitute(where), substitute(by), na.rm=T)
        },

        quantile = function(probs, columns=is.numeric, where=NULL, by=NULL) {
            browser() #- NOT WORKING probs not substituting...
            private$apply_expr(quantile, substitute(columns), substitute(where), substitute(by), na.rm=T, probs = eval(probs))
        },

        apply = function(fun, columns=NULL, where=NULL, by=NULL, ...) {
            call <- substitute(private$df$data[, lapply(.SD, fun, ...), .SDcols = cols, keyby=by])
            condition <- substitute(where)
            if (!is.null(condition)) call[[3]] <- condition
            call[[".SDcols"]] <- private$parse_sdcols(substitute(columns))
            eval(call)
        },

        summary = function(type=is.numeric) {browser()
            result = list(numeric, factor, character)
            na_rate = function(x) {mean(is.na(x))} #TODO How to get this function working in aggregate

            res_num <- self$aggregate(funs = list(min(x, na.rm=T), median(x, na.rm=T),
                                      mean(x, na.rm=T), max(x, na.rm=T), na_rate(x)),
                           columns = is.numeric)

            fct_cols <- names(private$df$data)[sapply(names(private$df$data), is.factor)]
            if (length(fct_cols) > 0) {
                for (i in fct_cols) private$df$count(by=i)
            }
            res_fct <- list()

        },

        copy = function() {
            StatFrame$new(private$df$copy())
        }
    ),

    active = list(
        data = function() {
            private$df$data
        }
    ),

    private = list(
        df = NULL,

        apply_expr = function(fun, columns=NULL, where=NULL, by=NULL, ...) {
            call <- substitute(private$df$data[, lapply(.SD, fun, ...), .SDcols = cols, keyby=.by])
            if (!is.null(where)) call[[3]] <- where
            if (is.call(columns) && (columns[[1]] == quote(.) || columns[[1]] == quote(list))) {
                columns[[1]] <- quote(c)
                for (i in 2:length(columns)) columns[[i]] <- deparse(columns[[i]])
            }
            call[[".SDcols"]] <- columns
            call[["keyby"]] <- by
            eval(call)
        },

        parse_sdcols = function(e) {
            if (is.call(e) && (e[[1]] == quote(.) || e[[1]] == quote(list))) {
                e[[1]] <- quote(c)
                for (i in 2:length(e)) e[[i]] <- deparse(e[[i]])
            }
            return(e)
        },

        finalize_aggregate = function(result, funs_expr, by_expr) {
            f_names <- vector("character", length = length(funs_expr)-1)
            for (i in seq_along(f_names)) f_names[i] <- deparse(funs_expr[[i+1]][[1]])
            result[, fun := rep(f_names, dim(result)[1L] %/% length(f_names))]
            data.table::setcolorder(result, c(private$listexpr_to_vector(by_expr), "fun"))
        },

        listexpr_to_vector = function(e) {
            if (is.null(e)) return(NULL)
            if (!is.null(names(e))) return(names(e)[-1L])
            e <- e[-1L]
            res <- vector("character", length = length(e))
            for (i in seq_along(res)) res[i] <- deparse(e[[i]])
            res
        }

    )
)
