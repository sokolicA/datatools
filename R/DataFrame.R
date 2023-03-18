DataFrame <- R6::R6Class(
    "DataFrame",
    public = list(
        initialize = function(tbl, primary_key = NULL, id = NULL) {
            data.table::setDT(tbl, key = primary_key)
            private$.tbl <- tbl
            private$.id <- id
        },

        get_data = function() {
            private$.tbl
        }

    ),

    private = list(
        .tbl = NULL,
        .id = NA_character_
    )
)
