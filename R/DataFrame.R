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
        },

        get_key = function() {
            data.table::key(private$.tbl)
        },

        set_key = function(key) {
            data.table::setkeyv(x=private$.tbl, cols=key)
        },

        is_key_unique = function() {
            uniqueN(private$.tbl, by = get_key(private$.tbl)) == nrow(private$.tbl)
        },

        count = function(by="") {
            private$.tbl[, .N, keyby = by]
        },

        append = function(rows) {

        },

        extract = function(where) {

        }

    ),

    private = list(
        .tbl = NULL,
        .id = NA_character_
    )
)
