DataFrame <- R6::R6Class(
    "DataFrame",
    public = list(
        initialize = function(tbl, key = NULL, id = NULL) {
            data.table::setDT(tbl, key = key)
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
