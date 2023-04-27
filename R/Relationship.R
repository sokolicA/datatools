#' @title Relationship Class
#'
#' @description A class for relationships between objects of class `data.frame`.
#'
#' @param left Optional object of class `data.table` (or `data.frame`, see Details) that is treated as the left table.
#' @param right Optional object of class `data.table` (or `data.frame`, see Details) that is treated as the right table.
#'
#' @details If the objects passed are not `data.table` objects then a `data.table` copy of the objects will be created with `data.table::as.data.table`.
#'
#'
#' @return A `Relationship` object.
#'
#' @export
Relationship <- R6::R6Class(
    "Relationship",
    public = list(
        #' @description Relationship constructor
        #'
        #' @param left Optional object of class `data.table` (or `data.frame`, see Details) that is treated as the left table.
        #' @param right Optional object of class `data.table` (or `data.frame`, see Details) that is treated as the right table.
        #' @param on The `on` clause specifying matching column names in left and right.
        initialize = function(left=NULL, right=NULL, on) {
            self$left <- left;
            self$right <- right;
            private$set_on(substitute(on))
        },

        #' @description Print method.
        #'
        print = function() {
          cat("Relationship on", deparse1(private$on_clause), "between:\n")
            print(head(private$x[, 1:min(ncol(private$x), 5)], 5))
            print(head(private$y[, 1:min(ncol(private$y), 5)], 5))
        },

        #' @description Reverse the left and right tables.
        #'
        #' @return A new Relationship object with reversed tables.
        reverse = function() {
            on <- private$swap_on_sub()
            eval(substitute(Relationship$new(self$right, self$left, on)))
        },

        #' @description Check if relationship is special.
        #'
        #' @return Logical TRUE or FALSE.
        is_special = function() {
            if (is.null(private$on_clause)) return(FALSE)
            #special <- c(">", ">=", "<", "<=")
            #any(sapply(special, grepl, deparse1(private$on_clause)))
            grepl(">|<", deparse1(private$on_clause))
        },

        #' @description Relating columns from the right table.
        #'
        #' @return A character vector of column names.
        right_on = function() {
            result = character(length=length(private$on_clause)-1)
            for (i in seq_along(private$on_clause[-1L])) {
                result[i] <- deparse(private$on_clause[[i+1L]])
            }
            if (self$is_special()) result <- gsub(".*[=|<|>] *", "", result)
            result
        },

        #' @description Check if the relationship is not fully specified.
        #'
        #' @return `TRUE` if `left`, `right` and `on` are specified and `FALSE` otherwise.
        #'
        is_not_fully_specified = function() {
            return(any(is.null(private$x), is.null(private$y), is.null(private$on_clause)))
        }
    ),

    active = list(
        #' @field left The left table.
        left = function(x) {
            if (missing(x)) return(private$x)
            if (!inherits(x, "data.table")) setDT(x)
            private$x <- x
        },

        #' @field right The right table.
        right = function(x) {
            if (missing(x)) return(private$y)
            if (!inherits(x, "data.table")) setDT(x)
            private$y <- x
        },

        #' @field on The on clause.
        on = function() private$on_clause
    ),

    private = list(
        x = NULL,
        y = NULL,
        on_clause = NULL,

        set_on = function(e) {
            private$on_clause <- if (is.null(e)) NULL else private$add_missing_on_names(e)
        },

        add_missing_on_names = function(e) {
            if (is.character(e)) return(private$add_missing_vec_names(e))
            if (e[[1L]] == quote(c)) return(private$add_missing_vec_names(eval(e)))
            if (is.call(e)) return(private$add_missing_expr_names(e))
        },

        add_missing_expr_names = function(e) {
            result <- if (!is.null(names(e))) names(e) else vector("character", length=length(e))
            missing <- which(result == "")[-1L]
            if (any(missing)) {
                for (i in missing) {
                    val <- deparse(e[[i]])
                    result[i] <- if(grepl(">|<", val)) "" else val
                }
                names(e) <- result
            }
            e
        },

        add_missing_vec_names = function(e) {
            result <- if (!is.null(names(e))) names(e) else vector("character", length=length(e))
            missing <- which(result == "")
            if (any(missing)) {
                for (i in missing) result[i] <- e[i]
                names(e) <- result
            }
            e
        },

        swap_on_sub = function() {
            if(is.character(private$on_clause)) return(private$swap_names_values(private$on_clause))
            result <- private$on_clause
            new_names <- names(private$on_clause)

            before <- c(">", ">=", "<", "<=")
            after <- c("<=", "<", ">=", ">")

            for (i in seq_along(result)[-1L]) {
                val <- deparse1(result[[i]])
                special <- sapply(before, grepl, val)
                if (any(special)) {
                    res <- unlist(strsplit(val, before[special]))
                    res <- paste(res[2], after[special], res[1])
                    new_names[i] <- res
                    val <- ""
                }
                result[[i]] <- str2lang(new_names[i])
                new_names[i] <- val
            }
            names(result) <- new_names
            result
        },

        swap_names_values = function(vec) {
            if (!is.character(vec)) stop("Must provide a character vector!")
            result <- names(vec)
            names(result) <- vec
            return(result)
        }
    )
)
