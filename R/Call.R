#' @title `data.table` call.
#'
#' @description Friend class of the `DataFrame` (has access to the private environment).
#'
#' @import R6
Call <- R6::R6Class(
    "Call",
    lock_class=TRUE,
    portable=TRUE,
    cloneable=TRUE,

    public = list(
        #' @description Constructor.
        #'
        #' @param tbl_env Optional environment containing a `data.table` object named `tbl`. See details.
        #' @param depth Optional default number of frames after the caller environment. Defaults to 2.
        #'
        #' @details
        #' By passing `tbl_env` you are specifying that the call is to be built
        #' for the `tbl` object contained in the environment.
        #' Additional checks for the validity of the changes to the call will be made.
        #'
        initialize = function(tbl_env=NULL, depth=2L) {
            private$set_tbl_env(tbl_env)
            private$set_depth(depth)
            private$start_call()
            private$verbose <- getOption("DataFrame.verbose", FALSE)

            invisible(self)
        },

        #' @description Print method.
        #'
        print = function() {
            print(private$expr)
            invisible(self)
        },

        #' @description Set or remove arguments to call.
        #'
        #' @param ... Named language objects. See details.
        #' @param env The environment used to set the arguments.
        #'
        #' @details
        #' In order to decrease the amount of ambiguity when evaluating objects
        #' using a DataMask, the following rules must be followed:
        #'  1. Column names must be passed as symbols. Example: `a`.
        #'  2. Variables must be passed as arguments to the `.v` function. Example: `.v(variable)`.
        #'  3. Functions must be passed as arguments to the `.f` function. Example: `.f(is.integer)`.
        #'
        set = function(..., env=parent.frame(private$depth)) {#browser()
            args <- list(...)
            private$assert_named(args)

            private$set_env(env)

            private$add_parsed(args)

            invisible(self)
        },


        #' @description Get the call expression.
        #'
        get = function() {
            private$expr
        },

        #' @description Evaluate the call.
        #'
        #' @param env The environment in which to evaluate.
        #'
        eval = function(env=parent.frame(private$depth)) {#browser()
            eval_env <- private$build_eval_env(env)
            call <- private$build_call()
            if (private$verbose) message("Evaluating: ", deparse1(call))
            result <- eval(call, envir=eval_env, enclos=eval_env)

            if (private$verbose && private$is_update()) {
                message("Rows affected: ", .Last.updated)
            }
            private$reset()

            result
        },

        #' @description Get a specific argument.
        #'
        #' @param name Name of the argument to get.
        #' @param as_chr Whether to return as character string.
        #'
        arg = function(name, as_chr=FALSE) {
            if (!is_string(name)) stop("'name' must be a string!")
            result <- private$expr[[name]]
            if (as_chr && !is.null(result)) {
                result <- deparse1(result)
                if (name=="by" || name=="keyby") result <- gsub("(^list\\()|(\\)$)", "", result)
            }
            result
        },

        #' @description Subset the call.
        #'
        #' @param args Character vector of arguments to keep.
        #'
        subset = function(args) {
            private$expr <- private$expr[names(private$expr) %in% c("", "x", args)]
            if (is.null(args)) private$env <- NULL
            invisible(self)
        },

        #' @description Get the used grouping.
        #'
        #' @param as_chr Whether to return as character string.
        #'
        grouping = function(as_chr=FALSE) {
            result <- self$arg("by")
            if (is.null(result)) result <- self$arg("keyby")
            if (as_chr && !is.null(result)) result <- gsub("(^list\\()|(\\)$)", "", deparse1(result))
            result
        },

        #' @description Reverse the on expression.
        #'
        reverse_on = function() {
            private$expr[["on"]] <- private$.reverse_on(private$expr[["on"]])
            invisible(self)
        },

        #' @description Get the column names defined by .SDcols.
        #'
        #' @param env The environment in which to evaluate.
        #'
        selected_columns = function(env=parent.frame(private$depth)) {
            if (is.null(private$tbl_env)) stop("Can not evaluate without tbl_env!")
            call <- self$clone()$subset(c(".SDcols"))$set(i=0, j=quote(.SD), env=env)
            names(call$eval(env))
        }
    ),

    private = list(

        tbl_env = NULL,

        expr = NULL,

        env = NULL,

        verbose = NULL,

        depth = NULL,

        start_call = function() {
            tbl <- if (is.null(private$tbl_env)) quote(x) else quote(.__private__$tbl)
            private$expr <- call("[", x=tbl)
        },

        assert_named = function(args) {
            if (is.null(names(args)) || any(names(args)==""))
                stop("All arguments must be named!", call.=FALSE)
        },

        set_tbl_env = function(x) {
            if (is.null(x)) return(NULL)
            if (!(is.environment(x) && inherits(x$tbl, "data.table"))) {
                stop("Must provide an environment containing a data.table named 'tbl'!", call.=FALSE)
            }
            private$tbl_env <- x
        },

        set_depth = function(x) {
            if (!is.integer(x)) stop("Depth must be integer!", call.=FALSE)
            private$depth <- x
        },

        set_env = function(env) {
            private$env <- if (is.null(env)) {
                env
            } else if (!is.environment(env)) {
                stop("'env' must be an environment!", call.=FALSE)
            } else {
                unequal_env <- is.environment(private$env) && !identical(env, private$env)
                if (unequal_env) stop("Call environment can not change! ",
                                      sub(".*(\\[.*)", "\\1", deparse1(private$expr)),
                                      " was set in environment '",
                                      data.table::address(private$env),
                                      "'. Use the 'reset_setup' method to reset the call.",
                                      call.=FALSE)
                env
            }
        },


        build_call = function() {
            if (any(names(private$expr) %in% c("i", "j"))) private$expr else private$expr[["x"]]
        },

        build_eval_env = function(env) {
            eval_env <- new.env(parent = env)
            eval_env$.__private__ <- private$tbl_env
            eval_env$.v <- function(x) {
                #.v's environment is the evaluation environment of buil_eval_env,
                # in which env is the caller environment. Since this is the next
                # environment on the search path (after the evaluation environment
                # of .v), x will be searched in the caller environment and its
                # parents.
                get(substitute(x), envir=env, inherits=TRUE)
            }
            eval_env
        },

        reset = function() {
            self$subset(NULL)
        },

        add_parsed = function(args) {
            for (arg in names(args)) {
                if (is.null(private$expr[[arg]]) && is.null(args[[arg]])) next;
                if (arg=="by" || arg=="keyby") private$set_handle_by(arg);
                parser <- private$find_parser(arg)
                private$expr[[arg]] <- parser(args[[arg]])
            }
        },

        find_parser = function(arg) {#browser()
            result <- try(get(paste0("parse_", arg), envir=private, inherits=FALSE), silent = TRUE)
            if (inherits(result, "try-error")) stop("Can not set ", arg, "!", call.=FALSE)
            result
        },

        parse_x = function(arg) {
            if (is.null(arg)) stop("Can not set 'x' to NULL.", call.=FALSE)
            if (!is.null(private$tbl_env)) stop("Can not set 'x' when 'tbl_env' is provided!", call.=FALSE)
            arg
        },

        parse_i = function(arg) {#browser()
            # Rules:
            # - If tbl_env is set enforce symbols as column names.
            # - If tbl_env is not set then warn if object of the same name is found in env
            # - functions not allowed (only function calls())
            if (is.atomic(arg)) return(arg)

            if (is.symbol(arg)) {
                if (private$is_function(arg)) stop("Can not pass functions to 'i'.", call.=FALSE)
                if (arg==quote(.SD) || arg==quote(.I)) return(arg)
                if (is.null(private$tbl_env)) {
                    if (exists(arg, envir=private$env, inherits=TRUE)) {
                        warning("Object '", arg, "' found in the calling environment! Use .v(", arg, ") to make the intent clear.",  call.=FALSE)
                    }
                    return(arg)
                } else {
                    if (private$is_column(arg)) return(arg)
                    stop("Only column names can be passed as symbols!", call.=FALSE)
                }
            }

            if (length(arg) == 1) stop("Unable to parse expression ", deparse1(arg), "!", call.=FALSE)
            if (arg[[1]] == quote(.v)) return(arg)
            if (!private$is_function(arg[[1]])) stop("First element of expression must be a function!", call.=FALSE)
            if (private$is_extraction(arg)) return(arg)

            for (i in seq_along(arg)[-1L]) arg[[i]] <- private$parse_i(arg[[i]])

            arg
        },

        parse_j = function(arg) {
            arg
        },

        parse_by = function(arg) {#browser()
            for (i in seq_along(arg)[-1L]) {
                arg[[i]] <- private$parse_by_inner(arg[[i]])
            }
            arg
        },

        parse_by_inner = function(arg) {#browser()
            # Rules:
            #  - symbols are treated as column names
            #  - variables are parts of the expression surrounded with .v()
            #  - atomic types are treated as such

            if (is.atomic(arg)) return(arg)
            if (is.symbol(arg)) {
                if (is.null(private$tbl_env)) {
                    if (exists(arg, envir=private$env, inherits=TRUE)) {
                        if (private$is_function(arg)) stop("Can not pass functions to 'by'.", call.=FALSE)
                        warning("Object '", arg, "' found in the calling environment!",  call.=FALSE)
                    }
                    return(arg)
                } else {
                    if (private$is_column(arg)) return(arg)
                    if (private$is_function(arg)) stop("Can not pass functions to 'by':", arg, ".", call.=FALSE)
                    stop("Only column names can be passed as symbols!", call.=FALSE)
                }
            }

            if (length(arg) == 1) stop("Unable to parse expression ", deparse1(arg), "!", call.=FALSE)
            if (arg[[1]] == quote(.v)) return(arg)
            if (!private$is_function(arg[[1]])) stop("First element of expression must be a function!", call.=FALSE)
            if (private$is_extraction(arg)) return(arg)

            for (i in seq_along(arg)[-1L]) arg[[i]] <- private$parse_by_inner(arg[[i]])

            arg
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
            for (i in seq_along(arg)[-1L]) {
                arg[[i]] <- private$parse_.SDcols_inner(arg[[i]])
            }
            if (length(arg)==2L) return(arg[[2L]])

            return(private$parse_.SDcols_combine(arg))
        },

        parse_.SDcols_inner = function(arg, sym_to_char=TRUE) {
            # arg can be:
            # 1. character column names or numeric positions - is.character, is.numeric
            # 2. startcol:endcol
            # 3. .SDcols=patterns(r5egex1, regex2, ...) - evaluated on names
            # 3. .SDcols=is.numeric - evaluated on columns
            # 4. Inversion (column dropping instead of keeping) can be accomplished be prepending the argument with ! or -

            if (is.numeric(arg) || is.character(arg)) return(arg)
            if (private$is_function(arg)) return(arg)
            if (is.symbol(arg)) return(private$parse_.SDcols_sym(arg, sym_to_char))

            if (length(arg) <= 1L) stop("Unable to parse expression ", deparse1(arg), "!", call.=FALSE)
            if (arg[[1]] == quote(.v)) return(arg)
            if (arg[[1]] == quote(patterns)) return(arg)
            if (!private$is_function(arg[[1L]])) stop("First element of expression must be a function!", call.=FALSE)
            if (private$is_extraction(arg)) return(arg)

            for (i in seq_along(arg)[-1L]) {
                if (arg[[1]] != quote(`:`)) {
                    arg[[i]] <- private$parse_.SDcols_inner(arg[[i]], sym_to_char)
                } else {
                    arg[[i]] <- private$parse_.SDcols_inner(arg[[i]], FALSE)
                }
            }

            arg
        },

        parse_.SDcols_sym = function(arg, as_chr=TRUE) {
            if (is.null(private$tbl_env)) {
                if (exists(arg, envir=private$env, inherits=TRUE)) {
                    warning("Object '", arg, "' found in the calling environment! Use .v(", arg, ") to make the intent clear.",  call.=FALSE)
                }
                if (as_chr) return(as.character(arg)) else return(arg)
            }
            if (!private$is_column(arg)) stop("Only column names can be passed as symbols!", call.=FALSE)
            if (as_chr) return(as.character(arg)) else return(arg)
        },

        parse_.SDcols_combine = function(arg) {
            exclude <- character()
            result <- character()
            for (i in seq_along(arg)[-1L]) {
                if (length(arg[[i]])==1L) {
                    result <- if (is.character(arg[[i]])) {
                        c(result, arg[[i]])
                    } else {
                        sdcols <- arg[[i]]
                        call <- substitute(private$tbl_env$tbl[, names(.SD), .SDcols=sdcols])
                        result <- c(result, eval(call))
                    }
                    next;
                }

                if (arg[[i]][[1]] == quote(`!`) || arg[[i]][[1]] == quote(`-`)) {
                    sdcols <- arg[[i]][[2]]
                    call <- substitute(private$tbl_env$tbl[, names(.SD), .SDcols=sdcols])
                    exclude <- c(exclude, eval(call))
                    next;
                }

                if (is.null(private$tbl_env)) stop("Can not parse multiple without tbl_env!")
                sdcols <- arg[[i]]
                call <- substitute(private$tbl_env$tbl[, names(.SD), .SDcols=sdcols])
                result <- c(result, eval(call))
            }

            setdiff(unique(result), exclude)
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

        is_update = function() {
            j <- private$expr[["j"]]
            !is.null(j) && length(j) > 1 && j[[1]] == quote(`:=`)
        },

        is_column = function(e) {
            if (is.null(private$tbl_env)) return(NULL)
            if (is.symbol(e)) {
                check <- try(eval(e, private$tbl_env$tbl, emptyenv()), silent=TRUE)
                return(!inherits(check, "try-error"))
            }
            if (is.character(e)) {
                return(is.element(e, names(private$tbl_env$tbl)))
            }
            FALSE
        },

        # Both functions and symbols that evaluate to functions!
        is_function = function(e) {
            is.function(try(eval(e, envir=private$env, enclos=private$env), silent=TRUE))
        },

        is_extraction = function(e) {
            e[[1]] == quote(`$`) || e[[1]] == quote(`[`) || e[[1]] == quote(`[[`)
        },

        set_handle_by = function(arg) {
            if (arg=="by" && !is.null(private$expr[["keyby"]])) {
                message("Removed previous group by key specification!", call.=FALSE)
                private$expr[["keyby"]] <- NULL
            }
            if (arg=="keyby" && !is.null(private$expr[["by"]])) {
                message("Removed previous group by specification!", call. = FALSE)
                private$expr[["by"]] <- NULL
            }
        },

        finalize = function() {

        }

    )
)
