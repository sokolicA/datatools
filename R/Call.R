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

        initialize = function(df) {
            stopifnot("Must provide an environment containing a data.table named 'tbl'!" = is.environment(df) && inherits(df$tbl, "data.table"))
            private$df <- df
            private$expr <- as.call(list(quote(`[`), x=quote(.__private__$tbl)))
            invisible(self)
        },

        print = function() {
            print(private$expr)
            invisible(self)
        },

        set = function(..., env=NULL) {#browser()
            #TODO do not allow env=NULL
            if (is.environment(env)) {
                unequal_env <- is.environment(private$env) && !identical(env, private$env)
                if (unequal_env) stop("Call environment can not change!", call.=FALSE)
                private$env <- env
            }

            args <- list(...)
            if (is.null(names(args)) || any(names(args)=="")) stop("All arguments must be named!", call.=FALSE)

            for (arg in names(args)) {
                arg_is_not_set_and_new_is_null <- is.null(private$expr[[arg]]) && is.null(args[[arg]])
                if (arg_is_not_set_and_new_is_null) next;

                parser <- private$parser(arg)
                private$expr[[arg]] <- parser(args[[arg]], env)
            }

            invisible(self)
        },

        eval = function(env) {#browser()
            if (missing(env)) stop("Must provide 'env' argument!")
            if (!is.environment(env)) stop("'env' must be an environment!")
            stopifnot("Evaluation environment must match the call environment!"=is.null(private$env) || identical(env, private$env))

            eval_env <- new.env(parent = env)
            eval_env$.__private__ <- private$df
            eval_env$.v <- function(x) {get(substitute(x), pos=1L, inherits=FALSE)}
            result <- eval(private$expr, envir=eval_env, enclos=eval_env)
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
        }

    ),

    private = list(

        df = NULL,

        expr = NULL,

        env = NULL,

        parser = function(arg) {
            result <- try(get(paste0("parse_", arg), envir=private, inherits=FALSE), silent = TRUE)
            if (inherits(result, "try-error")) stop("Can not set ", arg, "!", call.=FALSE)
            result
        },

        parse_i = function(e, env) {#browser()
            # Rules:
            #  - symbols are treated as column names
            #  - atomic types are treated as such
            #  - variables are parts of the expression surrounded with .v()
            if (is.atomic(e)) return(e)
            if (is.symbol(e)) {
                if (private$is_column(e)) return(e)
                stop("Only column names can be passed as symbols!", call.=FALSE)
            }

            if (length(e) == 1) stop("Unable to parse expression ", deparse1(e), "!", call.=FALSE)

            if (e[[1]] == quote(.v)) {
                #IDEA add ability to specify environment .(v, env)
                if (is.null(env)) stop("Environment must be provided for variables!", call.=FALSE)
                return(e)
            }

            if (private$is_extraction(e)) {
                ev <- try(eval(e, env), silent=TRUE)
                if (inherits(ev, "try-error") || is.null(ev)) stop(attr(ev, "condition")$message, call.=FALSE)
                e[[2]] <-  private$parse_i(e[[2]], env)
                return(e)
            }

            f <- eval(e[[1]], env)
            if (!is.function(f)) stop("First element of expression must be a function!", call.=FALSE)
            if (length(e[[1]]) > 1) stop("Currently unable to parse functions inside other objects.", call. = FALSE)


            # if (!any(is.primitive(f), isNamespace(environment(f)))) {
            #     f_chr <- deparse1(e[[1]])
            #     e[[1]] <- call("get", f_chr, pos=1L)
            # }


            for (i in seq_along(e)[-1L]) {
                e[[i]] <- private$parse_i(e[[i]], env)
            }

            e

        },

        parse_j = function(arg, env) {
            arg
        },

        parse_by = function(arg, env) {#browser()
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

                if (private$is_function(arg[[i]][[1]], env)) {
                    #TODO add check for arguments!
                    next
                } else stop("Do not know how to interpret given grouping: ", deparse1(arg[[i]]), ".")
            }
            return(arg)
        },

        parse_keyby = function(arg, env) {
            private$parse_by(arg, env)
        },

        parse_nomatch = function(arg, env) {
            arg
        },

        parse_mult = function(arg, env) {
            arg
        },

        parse_.SDcols = function(arg, env) {
            arg
        },

        parse_on = function(arg, env) {
            arg
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
            !inherits(try(eval(e, private$df$tbl, emptyenv()), silent=TRUE), "try-error")
        },

        is_symbol = function(e, env) {
          is.symbol(e) && !is.function(try(eval(e, envir=env, enclos=env), silent=TRUE))
        },

        is_function = function(e, env) {
          is.function(try(eval(e, envir=env, enclos=env), silent=TRUE))
        },

        finalize = function() {

        }

    )
)