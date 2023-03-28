Join <- R6::R6Class(
    "Join",
    public = list(
        initialize = function(left, right) {
            # TODO assert that data.table. or DataFrame?
            private$left <- left
            private$right <- right
            private$name_left <- deparse(substitute(left))
            private$name_right <- deparse(substitute(right))
        },

        print = function() {
            cat("Join Object")
            cat("\nJoining", private$name_left, "and", private$name_right)
            cat("\nJoining on: ", private$join_on)
            cat("\nAdding columns: ", private$add_columns)
        },

        on = function(...) {
            join_on <- private$parse_arguments(deparse(match.call(expand.dots=TRUE)))
            private$join_on <- paste0("list", join_on)
            return(invisible(self))
        },

        add = function(...) {
            private$add_columns <- private$parse_arguments(deparse(match.call(expand.dots=TRUE)))

            private$execute(private$command())
        },

        add_all = function(suffix="_r") {
            add <- names(private$right)
            duplicated <- add %in% names(private$left)
            add_names <- ifelse(duplicated, paste0(add, suffix), add)
            add <- paste0("i.", add)
            result <- paste0("(", paste0(paste0(add_names, "=", add), collapse =", "), ")")
            private$add_columns <- result
            private$execute(private$command())
        }
    ),
    private = list(
        left = NULL,
        right = NULL,
        name_left = NULL,
        name_right = NULL,
        join_on = NULL,
        add_columns = NULL,

        parse_arguments = function(x) {
            result <-  gsub(".*\\$(.*)$", "\\1", x)
            result <- gsub("^[^\\(]*", "", result)
            return(result)
        },

        command = function() {
            stop("Method must be used in subclass!")
        },

        execute = function(cmd) {
            eval(parse(text = cmd))
        }
    )
)

UpdateJoin <- R6::R6Class(
    "UpdateJoin",
    inherit = Join,
    public = list(

    ),

    private = list(
        command = function() {
            paste0(
                "private$left[private$right, on =", private$join_on,
                ", `:=` ", private$add_columns, "]"
            )
        }
    )
)

LeftJoin <- R6::R6Class(
    "LeftJoin",
    inherit = Join,
    public = list(

    ),

    private = list(
        command = function() {
            paste0(
                "private$right[private$left, on =", private$join_on,
                ", ", private$add_columns, "]"
            )
        }
    )
)
