StatFrame <- R6::R6Class(
    "StatFrame",
    public = list(
        initialize = function(data) {
            private$.data <- data
        },

        data = function() {
          private$.data$data
        },

        select = function(columns) {
            new <- self$clone()
            new_env <- .subset2(new, ".__enclos_env__")$private
            new_env$selected <- columns
            return(new)
        },

        group = function(by) {
            new <- self$clone()
            new_env <- .subset2(new, ".__enclos_env__")$private
            new_env$group_by <- by
            return(new)
        },

        is_grouped = function() {
          private$group_by != ""
        },

        count = function() {
            private$.data$count(by = private$group_by)
        },

        sum = function() {

        },

        median = function() {

        },

        summarize = function(statistic) {

        },

        filter = function(where) {

        },

        left_join = function() {

        },

        deep_clone = function() {
            result <- self$clone(deep=TRUE)
            result_private <- .subset2(result, ".__enclos_env__")$private
            result_private$.data <- private$.data$deep_clone()
            return(result)
        }
    ),

    private = list(
        .data = NULL,
        group_by = ""
    )
)

#data$select$group_by$sum
