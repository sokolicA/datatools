
test_private <- function(method, class, debug=FALSE) {
    result <- R6::R6Class(paste0("Test", class$classname), inherit=class)
    f <- class$private_methods[[method]]
    if (debug) f <- with_debugger(f)
    result$set("public", paste0("priv_", method), value=f, overwrite = TRUE)
    result
}

with_debugger <- function(f) {
    extend_body(f, quote(browser()), 1L)
}
#
# f1 <- mean
# f2 <- is.integer # error
# f3 <- function(x) x
# f4 <- function(x) {x}


