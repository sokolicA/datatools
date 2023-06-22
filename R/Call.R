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

        parse_i = function(arg, env) {
            arg
        },

        parse_j = function(arg, env) {
            arg
        },

        parse_by = function(arg, env) {
            arg
        },

        parse_keyby = function(arg, env) {
            arg
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

        finalize = function() {

        }

    )
)
