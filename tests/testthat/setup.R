
test_private <- function(method, class, debug=FALSE) {
    result <- R6::R6Class(paste0("Test", class$classname), inherit=class)
    f <- class$private_methods[[method]]
    if (debug) f <- with_debugger(f)
    result$set("public", paste0("priv_", method), value=f, overwrite = TRUE)
    result
}

with_debugger <- function(f) {
    b <- body(f)
    if (is.null(b)) stop("Function has no body!")

    if (length(b) == 1 || b[[1]] != quote(`{`)) b <- call("{", b)
    nb <- as.call(append(as.list(b), quote(browser()), after=1))

    body(f) <- nb
    f
}
#
# f1 <- mean
# f2 <- is.integer # error
# f3 <- function(x) x
# f4 <- function(x) {x}
