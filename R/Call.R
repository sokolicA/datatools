#' @title `data.table` call.
#'
#' @description Friend class of the `DataFrame` (has access to the private environment).
#'
#' @import R6
Call <- R6::R6Class(
    "Call",
    lock_class=TRUE,
    portable=TRUE,

    public = list(

        initialize = function() {
            private$start_call(quote(x))
            invisible(self)
        },

        print = function() {
            print(private$expr)
            invisible(self)
        },

        use = function(df) {
            private$validate_df(df)
            private$df <- df
            private$start_call(quote(.__private__$tbl))
            invisible(self)
        },

        set = function(..., env=parent.frame(2L)) {#browser()
            args <- list(...)
            private$assert_equal_env(env)
            private$assert_named(args)
            private$env <- env
            private$add_parsed(args)
            invisible(self)
        },

        get = function() {
            private$expr
        },

        eval = function(env=parent.frame(2L)) {#browser()
            private$assert_equal_env(env)

            eval_env <- private$build_eval_env(env)
            call <- private$build_call()
            result <- eval(call, envir=eval_env, enclos=eval_env)

            private$reset()
            result
        },

        arg = function(name) {
            if (!is_string(name)) stop("'arg' must be a string!")
            private$expr[[name]]
        },

        subset = function(args) {
            private$expr <- private$expr[names(private$expr) %in% c("", "x", args)]
            invisible(self)
        },

        grouping = function() {
            result <- self$arg("by")
            if (is.null(result)) result <- self$arg("keyby")
            result
        },

        reverse_on = function() {
            private$expr[["on"]] <- private$.reverse_on(private$expr[["on"]])
        }
    ),

    private = list(

        df = NULL,

        expr = NULL,

        env = NULL,

        start_call = function(x) {
            private$expr <- call("[", x=x)
        },

        validate_df = function(x) {
            if (!(is.environment(x) && inherits(x$tbl, "data.table"))) {
                stop("Must provide an environment containing a data.table named 'tbl'!", call.=FALSE)
            }
        },

        assert_equal_env = function(env) {
            if (is.null(private$df)) return(NULL)
            if (!is.environment(env)) stop("'env' must be an environment!", call.=FALSE)
            unequal_env <- is.environment(private$env) && !identical(env, private$env)
            if (unequal_env) stop("Call environment can not change!", call.=FALSE)
        },

        assert_named = function(args) {
            if (is.null(names(args)) || any(names(args)==""))
                stop("All arguments must be named!", call.=FALSE)
        },

        build_call = function() {
            if (any(names(private$expr) %in% c("i", "j"))) private$expr else private$expr[["x"]]
        },

        build_eval_env = function(env) {
            eval_env <- new.env(parent = env)
            eval_env$.__private__ <- private$df
            eval_env$.v <- function(x) {get(substitute(x), pos=1L, inherits=FALSE)}
            eval_env
        },

        add_parsed = function(args) {
            for (arg in names(args)) {
                if (is.null(private$expr[[arg]]) && is.null(args[[arg]])) next;
                if (arg=="by" || arg=="keyby") private$set_handle_by(arg);
                parser <- private$parser(arg)
                private$expr[[arg]] <- parser(args[[arg]])
            }
        },

        parser = function(arg) {
            result <- try(get(paste0("parse_", arg), envir=private, inherits=FALSE), silent = TRUE)
            if (inherits(result, "try-error")) stop("Can not set ", arg, "!", call.=FALSE)
            result
        },

        parse_x = function(arg) {
            if (!is.null(private$df)) stop("Can not set 'x' when using DataFrame!", call.=FALSE)
            arg
        },

        parse_i = function(arg) {#browser()
            # Rules:
            #  - symbols are treated as column names
            #  - atomic types are treated as such
            #  - variables are parts of the expression surrounded with .v()

            return(arg)

            if (is.atomic(e)) return(e)
            if (is.symbol(e)) {
                if (private$is_column(e) || e == quote(.SD)) return(e)
                stop("Only column names can be passed as symbols!", call.=FALSE)
            }

            if (length(e) == 1) stop("Unable to parse expression ", deparse1(e), "!", call.=FALSE)

            if (e[[1]] == quote(.v)) {
                #IDEA add ability to specify environment .(v, env)
                if (is.null(env)) stop("Environment must be provided for variables!", call.=FALSE)
                return(e)
            }

            if (private$is_extraction(e)) {
                ev <- try(eval(e, private$env), silent=TRUE)
                if (inherits(ev, "try-error") || is.null(ev)) stop(attr(ev, "condition")$message, call.=FALSE)
                e[[2]] <-  private$parse_i(e[[2]])
                return(e)
            }

            f <- eval(e[[1]], private$env)
            if (!is.function(f)) stop("First element of expression must be a function!", call.=FALSE)
            if (length(e[[1]]) > 1) stop("Currently unable to parse functions inside other objects.", call. = FALSE)


            # if (!any(is.primitive(f), isNamespace(environment(f)))) {
            #     f_chr <- deparse1(e[[1]])
            #     e[[1]] <- call("get", f_chr, pos=1L)
            # }


            for (i in seq_along(e)[-1L]) {
                e[[i]] <- private$parse_i(e[[i]])
            }

            e

        },

        parse_j = function(arg) {
            arg
        },

        parse_by = function(arg) {#browser()
            # Rules:
            #  - symbols are treated as column names
            #  - variables are parts of the expression surrounded with .v()
            #  - atomic types are treated as such
            for(i in seq_along(arg)[-1L]) {
                if (is.null(arg[[i]])) return(arg[[i]])

                if (is.character(arg[[i]])) arg[[i]] <- as.symbol(arg[[i]])
                if (is.symbol(arg[[i]])) {
                    if (private$is_column(arg[[i]])) next;
                    stop("Only column names can be passed as symbols!", call.=FALSE)
                }

                if (length(arg[[i]]) < 2) stop("Unable to parse by!")

                if (private$is_range(arg[[i]])) {
                    #CONSIDER allowing ranges
                    stop("Ranges are currently not supported!", call.=FALSE)
                }

                if (arg[[i]][[1]] == quote(.v)) next;

                if (private$is_function(arg[[i]][[1]])) {
                    #TODO add check for arguments!
                    next
                } else stop("Do not know how to interpret given grouping: ", deparse1(arg[[i]]), ".")
            }
            return(arg)
        },

        parse_keyby = function(arg) {
            private$parse_by(arg)
        },

        parse_nomatch = function(arg) {
            arg
        },

        parse_mult = function(arg) {
            arg
        },

        parse_.SDcols = function(arg) {
            # arg can be:
            # 1. character column names or numeric positions - is.character, is.numeric
            # 2. startcol:endcol
            # 3. .SDcols=patterns(r5egex1, regex2, ...) - evaluated on names
            # 3. .SDcols=is.numeric - evaluated on columns
            # 4. Inversion (column dropping instead of keeping) can be accomplished be prepending the argument with ! or -

            return(arg[[2]])

            if (is.character(e) || is.integer(e) || is.null(e)) return(e)
            if (private$is_range(e)) return(e)

            if (is.symbol(e) && private$is_function(e)) return(e)

            if (length(e) < 2) stop("Unable to parse select!", call.=FALSE)
            if (e[[1]] ==  quote(.v)) return(e)
            if (e[[1]] ==  quote(patterns)) return(e)
            if (e[[1]] == quote(`!`) || e[[1]] == quote(`-`)) {
                e[[2]] <- private$parse_sdcols(e[[2]])
                return(e)
            }

            stop("Do not know?")

        },

        parse_on = function(e) {
            # an unnamed character vector, c("a", "b"), used when columns a and b are common to both X and Y.
            # a named character vector, c(x1="y1", x2="y2"), used when join columns have different names (Foreign key joins).
            # string with a binary operator is also possible: c("x1==y1", "x2==y2")
            # a list (or it's dot alias): list(a, b), list(x1=y1, x2=y2)
            # non-equi joins are also possible: c("x>=a", "y<=b"), .(x>=a, y<=b)

            # CHECK .parse_on https://github.com/Rdatatable/data.table/blob/master/R/data.table.R
            if (is.null(e)) return(NULL)

            if (is.character(e)) {
                result <- call("c", e)
                names(result) <- c("", e)
                return(result)
            }

            if (e[[1L]] == quote(list) || e[[1L]] == quote(.)) {
                e <- private$convert_on_call(e)
            }

            if (e[[1L]] != quote(c)) stop("'on' argument should be a named vector of column names indicating which columns in self should be joined with which columns in other.", call.=FALSE)
            private$add_missing_on_expr_names(e)
        },

        convert_on_call = function(e) {
            ops <- c("==", "<=", "<", ">=", ">", "!=")
            pat <- paste0("(", ops, ")", collapse="|")
            spat <- paste0("[ ]+(", pat, ")[ ]+")
            # remove spaces around ==, >=,..
            e <- lapply(as.list(e)[-1L], function(x) gsub(spat, "\\1", deparse(x, width.cutoff=500L)))
            as.call(c(quote(c), e))
        },

        add_missing_on_expr_names = function(e) {
            result <- if (!is.null(names(e))) names(e) else vector("character", length=length(e))
            missing <- which(result == "")[-1L]
            if (any(missing)) {
                for (i in missing) result[i] <- sub("(=|!|>|<).*", "", e[[i]])
                names(e) <- result
            }
            e
        },

        .reverse_on = function(e) {
            result <- e
            result_names <- names(result)

            before <- c(">=", "<=", ">", "<", "!=")
            after <- c( "<", ">", "<=", ">=", "!=")

            for (i in seq_along(result)[-1L]) {
                val <- result[[i]]
                special <- sapply(before, grepl, val)
                if (any(special)) {
                    idx <- which.max(special)
                    res <- unlist(strsplit(val, before[idx]))
                    val <- res[2]
                    res <- paste0(res[2], after[idx], res[1])
                    result_names[i] <- res
                }
                result[[i]] <- result_names[i]
                result_names[i] <- val
            }
            names(result) <- result_names
            result
        },

        reset = function() {
            private$expr <- private$expr[1:2]
            private$env <- NULL
        },

        is_extraction = function(e) {
            e[[1]] == quote(`$`) || e[[1]] == quote(`[`) || e[[1]] == quote(`[[`)
        },

        is_range = function(e) {
            length(e) == 3 && e[[1]] == quote(`:`)
        },

        is_column = function(e) {
            if (is.null(private$df)) return(TRUE)
            !inherits(try(eval(e, private$df$tbl, emptyenv()), silent=TRUE), "try-error")
        },

        is_symbol = function(e) {
            is.symbol(e) && !is.function(try(eval(e, envir=private$env, enclos=private$env), silent=TRUE))
        },

        is_function = function(e) {
            is.function(try(eval(e, envir=private$env, enclos=private$env), silent=TRUE))
        },

        set_handle_by = function(arg) {
            if (arg=="by" && !is.null(private$expr[["keyby"]])) {
                warning("Removed previous group by key specification!", call.=FALSE)
                private$expr[["keyby"]] <- NULL
            }
            if (arg=="keyby" && !is.null(private$expr[["by"]])) {
                warning("Removed previous group by specification!", call. = FALSE)
                private$expr[["by"]] <- NULL
            }
        },

        finalize = function() {

        }

    )
)
