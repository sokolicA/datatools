DataModel <- R6::R6Class(
    "DataModel",
    public = list(
        initialize = function(data = NULL, relationship = NULL) {
            if (!is.null(data)) self$add_data(data)
            if (!is.null(relationship)) self$add_relationship(relationship)
        },

        add_data = function() {

        },

        add_relationship = function() {

        }

    ),

    private = list(
        .data = NULL,
        .relationships = NULL
    )
)
