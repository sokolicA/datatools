StaticEnv <- R6::R6Class(
    "StaticEnv",

    public = list(
        add = function(alias) {
            if (is.null(alias)) alias <- as.character(sample.int(1e8, 1))
            if (!is_string(alias)) stop("Alias must be a string!", call.=FALSE)
            if (exists(alias, private$env, inherits=FALSE)) stop("DataFrame with this alias already exists!", call.=FALSE)
            private$env[[alias]] <- 1
            private$count <- private$count + 1L
            alias
        },

        remove = function(alias) {
            if (!exists(alias, private$env, inherits=FALSE)) stop("DataFrame with this alias does not exist!", call.=FALSE)
            rm(list=alias, envir=private$env)
            private$count <- private$count - 1L
            TRUE
        },

        exists = function(alias) {
           exists(alias, envir=private$env, inherits=FALSE)
        },

        info = function() {
            list(count=private$count, list=names(private$env))
        }
    ),
    private = list(
        count = 0L,
        env = new.env()
    )
)
