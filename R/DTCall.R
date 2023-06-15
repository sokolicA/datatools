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
        #'
        #' @return Invisibly returns itself.
        #'
        set = function(...) {
            args <- list(...)
            #private$expr[names(args)] <- unlist(args) # looks more concise but does not work for removing elements.
            for (arg in names(args)) {
                if (is.null(private$expr[[arg]]) && is.null(args[[arg]])) next;
                private$expr[[arg]] <- args[[arg]]
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

        #' @description Get the call object.
        #'
        #' @param ... Named language (or symbol) arguments that are used in the `data.table` call.
        #'
        #' @return Invisibly returns itself.
        #'
        call = function(subset=NULL) {
            result <- private$expr
            if (is.character(subset)) result <- result[names(result) %in% c("", "x", subset)]
            if (any(names(result) %in% c("i", "j"))) return(result)
            result[["x"]]
        },

        #' @description Create a copy of the object.
        #'
        #' @param subset Optional subset of arguments to keep.
        #'
        #' @return Returns a new call object.
        #'
        copy = function(subset=NULL) {
            result <- DTCall$new()
            call <- self$call(subset)
            assign("expr", call, envir=.subset2(result, ".__enclos_env__")$private)
            return(result)
        }
    ),

    private = list(

        expr = NULL

    )
)


# For non-quoted arguments
# direct_set = function(...) {browser()
#     args <- substitute(list(...()))
#     private$expr[names(args)] <- args
#     invisible(self)
# }
