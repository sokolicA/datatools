StaticEnv <- R6::R6Class(
    "StaticEnv",

    public = list(
        add = function(name) {
            if (exists(name, private$names, inherits=FALSE)) stop("DataFrame with this name already exists!")
            private$names[[name]] <- 1
            private$count <- private$count + 1L
            TRUE
        },

        remove = function(name) {
            if (!exists(name, private$names, inherits=FALSE)) stop("DataFrame with this name does not exist!")
            rm(name, envir=private$names)
            private$count <- private$count - 1L
            TRUE
        },

        info = function() {
            list(count=private$count, names=names(private$names))
        }
    ),
    private = list(
        count = 0L,
        names = new.env()
    )
)
