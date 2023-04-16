#' @title StatFrame Class
#'
#' @description A wrapper around `DataFrame` or `data.frame` objects with statistical calculations available.
#' All the calculations return a `data.table` object.
#'
#' @import data.table
StatFrame <- R6::R6Class(
    "StatFrame",

    public = list(

        #' @description `StatFrame` Constructor
        #'
        #' @param df An object of class `DataFrame` or `data.frame`.
        #'
        initialize = function(df) {
            if (inherits(df, "data.frame")) {
                private$df <- DF(df)
            } else if (inherits(df, "DataFrame")) {
                private$df <- df
            } else {stop("Do not know how to perform statistics on such object type!")}
        },

        #' @description Print the table object.
        #'
        #' @details The method used is `print.data.table`.
        print = function() {
            print(private$df$unwrap())
        },

        #' @description Create a new `StatFrame` with filter applied to the table rows.
        #'
        #' @param keep An expression to be evaluated inside the table, integer vector specifying rows to remove or a logical vector. See details.
        #'
        #'
        #' @details
        #'\itemize{
        #' \item If an expression is passed it will be evaluated inside the context of the table.
        #' \item Passing duplicated numbers will duplicate such rows and passing numbers larger than the number of rows will result in `NA` rows..
        #' \item If a logical vector is passed it must be of the same length as the number of rows. Logical `NA` values are treated as `FALSE` and those rows will not be kept.
        #'}
        #'
        #' @return A new `StatFrame` with filtered rows.
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=1:5))
        #' sf$filter(a > 2)
        #' sf$filter(c(1, 3, 5))
        #' sf$filter(c(TRUE, NA, FALSE, FALSE, TRUE))
        filter = function(keep) {
            StatFrame$new(eval(substitute(private$df$unwrap()[keep])))
        },

        #' @description Count the number of rows.
        #'
        #' @param by An optional list() specifying the columns to group by. Defaults to no grouping.
        #'
        #' @return A keyed data.table with the row counts in the `N` column and the key to columns passed in `by`.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=1:5))
        #' sf$count()
        #' sf <- SF(data.frame(a=c(1,1,1,2,3), b=1:5))
        #' sf$count(by = list(a)) # df$count(by = .(a))
        count = function(by=NULL) {
            eval(substitute(private$df$count(by)))
        },

        #' Data aggregation
        #'
        #' @param funs A single function or a list of functions used to create an aggregate summary. See details.
        #' @param columns Optional list of columns to aggregate or a predicate. Defaults to all numeric columns.
        #' @param by By group.
        #'
        #' @details
        #' IMPORTANT: In order to use locally defined functions, they first have to be imported into the object with the `$import_function` method.
        #' This ensures that they are found in the execution path.
        #' Passing a named list of functions will result in using the names in the output. See examples.
        #'
        #'
        #' @return A `data.table`.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=6:10))
        #' sum_squares <- function(x) sum(x**2)
        #' # sf$import_function(sum_squares) # necessary only if sum_squares is defined locally
        #' sf$aggregate(list(sum_squares(x), mean(x), sd(x))) #mean and sd are always globally accessible
        #' sf$aggregate(list(max(x), mean(x)), by = .(cond = !b %in% c(6, 7)))
        #' sf <- SF(data.frame(a=c(1:5, NA), b=6:11))
        #' sf$aggregate(list(mean(x), mean_na_rm = mean(x, na.rm=T)))
        aggregate = function(funs, columns=is.numeric, by=NULL) {
            fexpr <- substitute(funs)
            if (grepl("function", deparse1(fexpr))) stop("Anonymous functions not currently supported!")
            call <- substitute(private$df$unwrap()[, lapply(lapply(.SD, function(x) {fexpr}), unlist), .SDcols = cols, keyby=by])
            call[[".SDcols"]] <- private$parse_sdcols(substitute(columns))
            result <- eval(call, envir=private$fun_env)
            private$finalize_aggregate(result, fexpr, substitute(by))
        },

        #' @description Sum the columns
        #'
        #' @param columns Optional list of columns to sum. Defaults to all numeric columns.
        #' @param by Grouping
        #'
        #' @details  `NA` values are not ignored.
        #'
        #' @return A `data.table` with the summed columns.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=6:10))
        #' sf$sum()
        #' sf$sum("a")
        sum = function(columns=is.numeric, by=NULL) {
            eval(substitute(private$df$unwrap()[, lapply(.SD, sum), .SDcols=columns, keyby = by]))
        },

        #' @description Calculate the mean of the columns
        #'
        #' @param columns Optional list of columns to calculate the mean. Defaults to all numeric columns.
        #' @param by Grouping
        #'
        #' @details  `NA` values are not ignored.
        #'
        #' @return A `data.table` with the means calculated.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=6:10))
        #' sf$mean()
        #' sf$mean("a")
        mean = function(columns=is.numeric, by=NULL) {
            eval(substitute(private$df$unwrap()[, lapply(.SD, mean), .SDcols=columns, keyby = by]))
        },

        #' @description Calculate the standard deviation of the columns
        #'
        #' @param columns Optional list of columns to calculate the standard deviation. Defaults to all numeric columns.
        #' @param by Grouping.
        #'
        #' @details  `NA` values are not ignored.
        #'
        #' @return A `data.table` with the standard deviations calculated.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=6:10))
        #' sf$sd()
        #' sf$sd("a")
        sd = function(columns=is.numeric, by=NULL) {
            eval(substitute(private$df$unwrap()[, lapply(.SD, sd), .SDcols=columns, keyby = by]))
        },

        #' @description Calculate the Median Absolute Deviation of the columns
        #'
        #' @param columns Optional list of columns to calculate the Median Absolute Deviation. Defaults to all numeric columns.
        #' @param by Grouping
        #'
        #' @details  `NA` values are not ignored.
        #'
        #' @return A `data.table` with the Median Absolute Deviation calculated.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=6:10))
        #' sf$mad()
        #' sf$mad("a")
        mad = function(columns=is.numeric, by=NULL) {
            eval(substitute(private$df$unwrap()[, lapply(.SD, mad), .SDcols=columns, keyby = by]))
        },

        #' @description Calculate the Interquartile Range of the columns
        #'
        #' @param columns Optional list of columns to calculate the Interquartile Range. Defaults to all numeric columns.
        #' @param by Grouping
        #'
        #' @details  `NA` values are not ignored.
        #'
        #' @return A `data.table` with the Interquartile Range calculated.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=6:10))
        #' sf$IQR()
        #' sf$IQR("a")
        IQR = function(columns=is.numeric, by=NULL) {
            eval(substitute(private$df$unwrap()[, lapply(.SD, IQR), .SDcols=columns, keyby = by]))
        },

        #' @description Calculate the median of the columns
        #'
        #' @param columns Optional list of columns to calculate the median. Defaults to all numeric columns.
        #' @param by Grouping
        #'
        #' @details  `NA` values are not ignored.
        #'
        #' @return A `data.table` with the medians calculated.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=6:10))
        #' sf$median()
        #' sf$median("a")
        median = function(columns=is.numeric, by=NULL) {
            eval(substitute(private$df$unwrap()[, lapply(.SD, median), .SDcols=columns, keyby = by]))
        },

        #' @description Calculate the minimum value of the columns
        #'
        #' @param columns Optional list of columns to calculate the minimum. Defaults to all numeric columns.
        #' @param by Grouping
        #'
        #' @details  `NA` values are not ignored.
        #'
        #' @return A `data.table` with the minimums calculated.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=6:10))
        #' sf$min()
        #' sf$min("a")
        min = function(columns=is.numeric, by=NULL) {browser()
            eval(substitute(private$df$unwrap()[, lapply(.SD, min), .SDcols=columns, keyby = by]))
        },

        #' @description Calculate the maximum value of the columns
        #'
        #' @param columns Optional list of columns to calculate the maximum. Defaults to all numeric columns.
        #' @param by Grouping
        #'
        #' @details  `NA` values are not ignored.
        #'
        #' @return A `data.table` with the maximum values calculated.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=6:10))
        #' sf$max()
        #' sf$max("a")
        max = function(columns=is.numeric, by=NULL) {
            eval(substitute(private$df$unwrap()[, lapply(.SD, max), .SDcols=columns, keyby = by]))
        },

        #' @description Calculate quantiles of columns
        #'
        #' @param columns Optional list of columns to calculate the quantiles Defaults to all numeric columns.
        #' @param by Grouping
        #' @param probs Vector of probabilities.
        #'
        #' @details  `NA` values are not ignored.
        #'
        #' @return A `data.table` with the quantiles calculated.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=6:10))
        #' sf$quantile()
        #' sf$quantile(columns="a")
        quantile = function(columns=is.numeric, by=NULL, probs=seq(0, 1, 0.25)) {
            result <- eval(substitute(private$df$unwrap()[, lapply(.SD, quantile, probs), .SDcols=columns, keyby = by]))
            result[, prob := rep(probs, nrow(result)%/% length(probs))]
            data.table::setcolorder(result, "prob")
            result[]
        },

        #' @description Summary on missing values.
        #'
        #' @param columns Optional selection of columns.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=c(1:5,NA, NA), b=c(1:6, NA)))
        #' sf$missing_data_summary()
        missing_data_summary = function(columns=NULL) {
            call <- substitute(private$df$unwrap()[, lapply(.SD, function(x) list(sum(is.na(x)),
                                                                                  round(mean(is.na(x))*100, 2))), .SDcols=columns])
            call[[".SDcols"]] <- substitute(columns)
            result <- eval(call)
            result <- as.data.table(t(result), keep.rownames = TRUE)
            setnames(result, c("column", "n_miss", "pct_miss"))
            result
        },

        #' @description Apply a function to columns
        #'
        #' @param fun Function to apply. See details.
        #' @param columns Optional list of columns to apply the function on. Can also be a predicate (e.g. `is.numeric`)
        #' @param ... Additional arguments passed to `fun`
        #'
        #' @details
        #' IMPORTANT: In order to use locally defined functions, they first have to be imported into the object with the `$import_function` method.
        #' This ensures that they are found in the execution path.
        #' @return A `StatFrame`.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=6:10))
        #' add_2 <- function(x) x+2
        #' # sf$import_function(add_2) # necessary only if sum_squares is defined locally
        #' sf$map(add_2) #mean and sd are always globally accessible
        #' sf$map(as.character)
        map = function(fun, columns=NULL, ...) {
            call <- substitute(private$df$unwrap()[, lapply(.SD, fun, ...), .SDcols = cols])
            call[[".SDcols"]] <- private$parse_sdcols(substitute(columns))
            SF(eval(call))
        },

        #' @description IN DEVELOPMENT. Show summary of the data
        #'
        #' @param col_type Optional type of columns to show the summary for. Defaults to all types.
        #'
        #' @return A `data.table`.
        #'
        #' @examples
        #' sf <- SF(data.frame(a=1:5, b=6:10))
        #' sf$describe()
        describe = function(col_type=NULL) {browser()
            result = list(numeric=NULL, logical=NULL, factor=NULL, character=NULL)
            self$import_function(function(x) {mean(is.na(x))}, "prop_missing")

            result$numeric <- self$aggregate(funs = list(min(x, na.rm=T), median(x, na.rm=T),
                                                         mean(x, na.rm=T), max(x, na.rm=T), prop_missing(x)),
                                             columns = is.numeric)

            fct_cols <- names(private$df$unwrap())[sapply(private$df$unwrap(), is.factor)]
            if (length(fct_cols) > 0) {
                res_fct <- vector("list", length = length(fct_cols))
                for (i in seq_along(fct_cols)) res_fct[[i]] <- private$df$unwrap()[, .N, by = c(fct_cols[i])]
                result$factor <- res_fct
            }

            result$character <- self$aggregate(funs = list(prop_missing(x)),
                                               columns = is.character)

            result

        },

        #' @description Import functions into the object.
        #'
        #' @param fun A function object. If the function is anonymous it must be named with the `name` argument.
        #' @param name The name of the function. Mandatory if the function is anonymous.
        #'
        #' @details
        #' The functions are stored in an environment that is used as an evaluation environment of some methods.
        #' http://adv-r.had.co.nz/Environments.html#function-envs
        #' https://stackoverflow.com/questions/57672803/apply-and-forceandcall-ignoring-get-from-parent-frame
        import_function = function(fun, name=NULL) {
            if (!is.function(fun)) stop("Must provide a function as fun!")
            if(is.null(private$fun_env)) private$fun_env <- new.env(parent=self$.__enclos_env__)
            if (!is.null(name) && make.names(name) != name) stop("Please provide a valid function name!")
            f_sub <- substitute(fun)
            if (is.call(f_sub)) {
                if (is.null(name)) stop("You must provide a name for the anonymous function!")
            } else if (is.null(name)) name <- deparse1(substitute(fun))
            private$fun_env[[name]] <- fun
        },


        #' @description Create a deep copy of the object.
        #'
        #' @return A deep copy of the object.
        copy = function() {
            StatFrame$new(private$df$copy())
        }
    ),

    active = list(
        #' @field DF The underlying `DataFrame` object.
        DF = function() {
            private$df
        },

        #' @field imported_functions List of functions imported with `$import_function`.
        imported_functions = function() mget(ls(private$fun_env), envir=private$fun_env)
    ),

    private = list(
        df = NULL,

        fun_env = NULL,

        parse_sdcols = function(e) {
            if (is.call(e) && (e[[1]] == quote(.) || e[[1]] == quote(list))) {
                e[[1]] <- quote(c)
                for (i in 2:length(e)) e[[i]] <- deparse(e[[i]])
            }
            return(e)
        },

        finalize_aggregate = function(result, f_expr, by_expr) {
            if (f_expr[[1]] == quote(list)) {
                f_expr <- f_expr[-1L]
                f_names <- if (!is.null(names(f_expr))) names(f_expr) else vector("character", length=length(f_expr))
                missing <- which(f_names=="")
                if (length(missing) > 0) {
                    for (i in missing) f_names[i] <- deparse(f_expr[[i]][[1]])
                }
            } else {f_names <- deparse(f_expr[[1]])}
            result[, fun := rep(f_names, times=dim(result)[1L] %/% length(f_names))]

            groups <- private$make_names_by(by_expr)
            #if (any(groups %in% names(private$df$unwrap()))) stop("Collision between some column and group (by) names!")
            if (!is.null(groups)) {data.table::setnames(result, seq_along(groups), groups)}
            data.table::setcolorder(result, c(groups, "fun"))

            result[]
        },

        make_names_by = function(e) {
            if (is.null(e)) return(NULL)
            e <- e[-1L]
            result <- if (!is.null(names(e))) names(e) else vector("character", length=length(e))
            if (all(result!="")) return(result)
            missing <- which(result=="")
            for (i in missing) result[i] <- deparse(e[[i]])
            result
        },

        map_expr = function(fun, columns=NULL, where=NULL, by=NULL, ...) {
            call <- substitute(private$df$unwrap()[, lapply(.SD, fun, ...), .SDcols = cols, keyby=.by])
            if (!is.null(where)) call[[3]] <- where
            if (is.call(columns) && (columns[[1]] == quote(.) || columns[[1]] == quote(list))) {
                columns[[1]] <- quote(c)
                for (i in 2:length(columns)) columns[[i]] <- deparse(columns[[i]])
            }
            call[[".SDcols"]] <- columns
            call[["keyby"]] <- by
            eval(call)
        }

    )
)
