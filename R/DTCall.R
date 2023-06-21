#' @title data.table call
#'
#' @description
#'
#' @import R6
DTCall <- R6::R6Class(
    "DTCall",
    cloneable = FALSE,

    public = list(

        #' @description Constructor.
        #'
        #' @param x Name of the data.table.
        #'
        #' @return Invisibly returns itself.
        #'
        initialize = function(x=private$tbl) {
            private$expr <- as.call(list(quote(`[`), x=substitute(x)))
            invisible(self)
        },

        #' @description Default print method.
        #'
        #' @return Invisibly returns itself.
        #'
        print = function() {
            print(private$expr)
            invisible(self)
        },

        #' @description Sets (or unsets) arguments in the `data.table` call.
        #'
        #' @param ... Named language (or symbol) arguments that are used in the `data.table` call.
        #' @param env Optional enclosing environment of the `data.table` call.
        #'
        #' @return Invisibly returns itself.
        #'
        set = function(..., env=NULL) {#browser()
            args <- list(...)
            if (!is.null(env)) {
                stopifnot("'env' must be an environment!" = is.environment(env))
                if (!(is.null(private$env) || identical(private$env, env))) {
                    warning("Environment has changed! Resetting previous call!")
                    private$expr <- private$expr[1:2]
                }
                private$env <- env
            }

            #private$expr[names(args)] <- unlist(args) # looks more concise but does not work for removing elements.
            for (arg in names(args)) {#browser()
                arg_is_not_set_and_new_is_null <- is.null(private$expr[[arg]]) && is.null(args[[arg]])
                if (arg_is_not_set_and_new_is_null) next;

                if (arg=="by") {
                    if (!is.null(private$expr[["keyby"]])) {
                        warning("Overriding previous group by key specification!")
                        private$expr[["keyby"]] <- NULL
                    }
                    private$expr[[arg]] <- private$parse_by(args[[arg]])
                } else if (arg=="keyby") {
                    if(!is.null(private$expr[["by"]])) {
                        warning("Overriding previous group by specification!")
                        private$expr[["by"]] <- NULL
                    }
                    private$expr[[arg]] <- private$parse_by(args[[arg]])
                } else if (arg=="on") {
                    private$expr[[arg]] <- private$parse_on(args[[arg]])
                } else if (arg==".SDcols") {
                    private$expr[[arg]] <- private$parse_sdcols(args[[arg]], env)
                } else {
                    private$expr[[arg]] <- args[[arg]]
                }
            }
            invisible(self)
        },

        #' @description Get the value of the argument in the `data.table` call.
        #'
        #' @param arg Name of the argument passed as a string.
        #'
        #' @return Value of the argument. Returns NULL if the argument is not set.
        #'
        get = function(arg) {
            if (!is_string(arg)) stop("'arg' must be a string!")
            private$expr[[arg]]
        },

        #' @description Get the `data.table` call.
        #'
        #' @param subset Optional subset of arguments to keep.
        #'
        #' @return Invisibly returns itself.
        #'
        call = function(subset=NULL) {
            result <- if (is.character(subset)) private$subset(subset) else private$expr
            if (any(names(result) %in% c("i", "j"))) return(result)
            result[["x"]]
        },

        #' @description Return the `data.table` call with its environment.
        #'
        #' @param subset Optional subset of arguments to keep.
        #'
        #' @return Invisibly returns itself.
        #'
        consume = function(subset=NULL) {
            call <- if (is.character(subset)) private$subset(subset) else private$expr
            result <- if (any(names(call) %in% c("i", "j"))) call else call[["x"]]
            private$env <- NULL
            private$expr <- call[1:2]
            enquo(result, private$env)
        },

        #' @description Create a copy of the object.
        #'
        #' @param subset Optional subset of arguments to keep.
        #'
        #' @return Returns a new call object.
        #'
        copy = function(subset=NULL) {
            result <- DTCall$new()
            call <- if (is.character(subset)) private$subset(subset) else private$expr
            assign("expr", call, envir=.subset2(result, ".__enclos_env__")$private)
            return(result)
        },

        #' @description Get the grouping argument in the call.
        #'
        #' @return Returns the value of the grouping argument.
        #'
        grouping = function() {
            result <- self$get("by")
            if (is.null(result)) result <- self$get("keyby")
            result
        },

        reverse_on = function() {
            private$expr[["on"]] <- private$.reverse_on(private$expr[["on"]])
        }
    ),

    private = list(

        expr = NULL,

        env = NULL,

        subset = function(args) {
            private$expr[names(private$expr) %in% c("", "x", args)]
        },

        parse_sdcols = function(e, env) {#browser() #TODO
            # e can be:
            # 1. character column names or numeric positions - is.character, is.numeric
            # 2. startcol:endcol
            # 3. .SDcols=patterns(r5egex1, regex2, ...) - evaluated on names
            # 3. .SDcols=is.numeric - evaluated on columns
            # 4. Inversion (column dropping instead of keeping) can be accomplished be prepending the argument with ! or -

            #TODO
            # sdcols <- private$parse_sdcols(e, parent.frame())
            # test_call <- DTCall$new()$set(i=0, j=quote(.SD), .SDcols=sdcols)$call()
            # private$eval(test_call, reset=FALSE)
            if (is.character(e) || is.integer(e) || is.null(e)) return(e)
            if (length(e) == 3 && e[[1]] == quote(`:`)) return(e)
            ev <- try(eval(e, env), silent=TRUE)
            if (!inherits(ev, "try-error")) {
                if (is.function(ev)) return(e)
                if (is.character(ev) || is.integer(ev)) return(ev)
            }

            if (length(e) < 2) stop("Unable to parse select!", call.=FALSE)
            if (e[[1]] ==  quote(patterns)) return(e)
            if (e[[1]] == quote(`!`) || e[[1]] == quote(`-`)) {
                e[[2]] <- private$parse_sdcols(e[[2]], env)
                return(e)
            }

            stop("Do not know?")

        },

        parse_by = function(e) {#browser()
            #TODO add check for validity?
            # check <- try(eval(substitute(private$tbl[0][, .N, by = result])), silent=TRUE)
            # if (inherits(check, "try-error")) stop(attr(check, "condition")$message)
            result <- quote(list())
            idx <- 2L
            for(i in seq_along(e)[-1L]) {
                el <- e[[i]]
                if (is.name(el)) {
                    result[[idx]] <- el
                    if (!is.null(names(e))) names(result)[[idx]] <- names(e)[i]
                    idx <- idx + 1L
                    next
                }
                if (is.null(el)) {
                    result <- NULL
                    break
                }
                if (is.character(el)) {
                    result[[idx]] <- as.name(el)
                    idx <- idx + 1L
                    next
                }
                if (!is.name(el) && el[[1]] == quote(c)) {
                    N <- 2L
                    while(TRUE) {
                        cols <- try(eval(el, parent.frame(n=N)), silent=TRUE)
                        if (!inherits(cols, "try-error")) break
                        if (identical(globalenv(), parent.frame(n=N))) {
                            stop(attr(cols, "condition")$message)
                        }
                        N <- N+1L
                    }
                    for (col in cols) {
                        result[[idx]] <- as.name(col)
                        idx <- idx + 1L
                    }
                    next
                }
                if (is.call(el) &&  grepl("!|>|<|=|%",  as.character(el[[1]]))) {
                    result[[idx]] <- el
                    if (!is.null(names(e))) names(result)[[idx]] <- names(e)[i]
                    idx <- idx + 1L
                    next
                }
                if (is.call(el)) {
                    result[[idx]] <- el
                    if (!is.null(names(e))) names(result)[[idx]] <- names(e)[i]
                    idx <- idx + 1L
                    next
                }
                stop("Do not know how to interpret given grouping: ", as.character(el), ".")
            }
            return(result)
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
        }
    )
)


# For non-quoted arguments
# direct_set = function(...) {browser()
#     args <- substitute(list(...()))
#     private$expr[names(args)] <- args
#     invisible(self)
# }
