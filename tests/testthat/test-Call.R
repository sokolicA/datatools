test_that("setting arguments in Call works",
          {
              e <- Call$new()
              e$set(i = quote(a==3), j = quote(.SD))

              expect_equal(e$get(), quote(`[`(x= x, i = a == 3, j = .SD)))
          })

test_that("renaming table does not work through Call$set if using DataFrame",
          {
              df <- DF(data.table(a=1:5))
              e <- Call$new()$use(df$.__enclos_env__$private)
              expect_error(e$set(x=quote(y)))
          })

test_that("renaming table works through Call$set if not using DataFrame",
          {
              e <- Call$new()
              expect_equal(e$set(x=quote(y))$get(), quote(`[`(x=y)))
          })

test_that("removing and overriding arguments in Call works",
          {
              e <- Call$new()
              e$set(i = NULL, j = NULL)
              expect_equal(e$get(), quote(`[`(x=x)))

              e$set(i = quote(a==3), j = quote(.SD))
              e$set(i = NULL, j = quote(`:=` (y = x)))
              expect_equal(e$get(), quote(`[`(x= x, j = `:=`(y = x))))
          })


test_that("getting arguments in Call works",
          {
              e <- Call$new()
              e$set(i = quote(a==3), j = quote(.SD))

              expect_error(e$arg(c("i", "j")))

              expect_null(e$arg(c("by")))

              expect_equal(e$arg("i"), quote(a == 3))
          })


test_that("call subset works",
          {
              e <- Call$new()
              e$set(i = quote(a==3), j = quote(.SD), by=quote(list(a)))

              expect_equal(e$get(), quote(`[`(x=x, i=a==3, j=.SD, by=list(a))))
              expect_equal(e$subset("i")$get(), quote(`[`(x=x, i=a==3)))
          })


test_that("by and keyby are mutually exclusive with message upon override",
          {
              e <- Call$new()
              e$set(i=quote(a), by=quote(list(a)))

              expect_message(e$set(keyby=quote(list(a))))
              expect_equal(e$get(), quote(`[`(x=x, i=a, keyby=list(a))))
              expect_message(e$set(by=quote(list(a))))
              expect_equal(e$get(), quote(`[`(x=x, i=a, by=list(a))))
          })







instantiate_call <- function(c) {
    df <- DF(data.frame(a=1:5, b = c("a", "a", "b", "c", "c"), c = 5:1))
    c$new()$use(df$.__enclos_env__$private)
}

preprocess <- function(...) substitute(list(...))



test_that("parse_by works", {
    c <- test_private("parse_by", Call, FALSE)
    test <- instantiate_call(c)

    a <- "b"

    g1 <- preprocess(a)
    g2 <- preprocess(a, b, c)
    g3 <- preprocess("a")
    g4 <- preprocess("a", "b")
    g5 <- preprocess(a, "b")
    g6 <- preprocess(a:b)
    g7 <- preprocess(.v(a), a)
    g8 <- preprocess(a>=2)
    g9 <- preprocess(b =a>=2)

    expect_equal(test$priv_parse_by(g1), quote(list(a)))
    expect_equal(test$priv_parse_by(g2), quote(list(a, b, c)))
    expect_equal(test$priv_parse_by(g3), quote(list(a)))

    expect_equal(test$priv_parse_by(g4), quote(list(a, b)))
    expect_equal(test$priv_parse_by(g5), quote(list(a, b)))

    expect_error(test$priv_parse_by(g6))

    expect_equal(test$priv_parse_by(g7), quote(list(.v(a), a)))

    expect_equal(test$priv_parse_by(g8), quote(list(a>=2)))
    expect_equal(test$priv_parse_by(g9), quote(list(b=a>=2)))
})


test_that("parse_.SDcols works", {
    c <- test_private("parse_.SDcols", Call, FALSE)
    test <- instantiate_call(c)

    expect_equal(test$priv_parse_.SDcols(preprocess("a")), quote("a"))
    expect_equal(test$priv_parse_.SDcols(preprocess(c(1,2,3))), quote(c(1,2,3)))
    expect_equal(test$priv_parse_.SDcols(preprocess(1:5)), quote(1:5))
    expect_equal(test$priv_parse_.SDcols(preprocess(a:b)), quote(a:b))
    expect_equal(test$priv_parse_.SDcols(preprocess(is.numeric)), quote(is.numeric))
    expect_equal(test$priv_parse_.SDcols(preprocess(patterns("a", "b"))), quote(patterns("a", "b")))
    expect_equal(test$priv_parse_.SDcols(preprocess(.v(a))), quote(.v(a)))
})



test_that("On argument parser works", {

    Test <- R6::R6Class(
        "Test",
        inherit=Call,
        public=list(
            t_parse_on = function(on) {
                # browser()
                e <- substitute(on)
                private$parse_on(e)
            }
        )
    )


    df <- Test$new()
    # EQUI JOINS
    expect_equal(df$t_parse_on("a"), quote(c(a = "a")))
    expect_equal(df$t_parse_on(c("a", "b")), quote(c(a="a", b="b")))
    expect_equal(df$t_parse_on(c(a="b", b="a")), quote(c(a="b", b="a")))
    expect_equal(df$t_parse_on(c(a="b", "b")), quote(c(a="b", b="b")))
    expect_equal(df$t_parse_on(list(a, b)), quote(c(a="a", b="b")))
    expect_equal(df$t_parse_on(.(a, b)), quote(c(a="a", b="b")))
    expect_equal(df$t_parse_on(list(a=b, b=a)), quote(c(a="b", b="a")))
    expect_equal(df$t_parse_on(list(a=b, b)), quote(c(a="b", b="b")))
    #df$t_parse_on(c("a==b", "b")) # maybe in the future

    # NON-EQUI JOINS
    expect_equal(df$t_parse_on(list(a>b)), quote(c(a = "a>b")))
    expect_equal(df$t_parse_on(list(a>b, b)), quote(c(a = "a>b", b="b")))
    expect_equal(df$t_parse_on(.(a>b)), quote(c(a = "a>b")))
    expect_equal(df$t_parse_on(.(a>b, b)), quote(c(a = "a>b", b="b")))
    expect_equal(df$t_parse_on(c("a>b")), quote(c(a = "a>b")))
    expect_equal(df$t_parse_on(c("a>b", "b")), quote(c(a = "a>b", b="b")))
})

test_that("reversing the on expression works", {
    Test <- R6::R6Class(
        "Test",
        inherit=Call,
        public=list(
            t_reverse_on = function(on) {
                #browser()
                # e <- substitute(on)
                private$.reverse_on(on)
            }
        )
    )
    cl <- Test$new()
    # EQUI JOINS
    expect_equal(cl$t_reverse_on(quote(c(a = "a"))), quote(c(a = "a")))
    expect_equal(cl$t_reverse_on(quote(c(a="a", b="b"))), quote(c(a="a", b="b")))
    expect_equal(cl$t_reverse_on(quote(c(a="b", b="a"))), quote(c(b="a", a="b")))
    expect_equal(cl$t_reverse_on(quote(c(a="b", b="b"))), quote(c(b="a", b="b")))

    # NON-EQUI JOINS
    expect_equal(cl$t_reverse_on(quote(c(b = "b!=a"))), quote(c(a = "a!=b")))
    expect_equal(cl$t_reverse_on(quote(c(a = "a>b"))), quote(c(b = "b<=a")))
    expect_equal(cl$t_reverse_on(quote(c(a = "a<=b"))), quote(c(b = "b>a")))
    expect_equal(cl$t_reverse_on(quote(c(a = "a>b", b="a"))), quote(c(b = "b<=a", a="b")))
})









#
# test_that("parse_i works", {
#
#     Test <- R6::R6Class(
#         "Test",
#         inherit=DataFrame,
#         public=list(
#             t_parse_i = function(e, env=parent.frame()) {
#                 #browser()
#                 private$parse_i(substitute(e), env)
#             }
#         )
#     )
#
#     chr <- c("a", "b")
#     log <- c(TRUE, FALSE)
#     lng <- quote(mean)
#     blt_1 <- `==`
#     blt_2 <- c
#     fun_base <- mean
#     is_greater_than_2 <- function(x) x > 2
#
#     tmp_env <- new.env(parent=emptyenv())
#     local({f <- function(x) "local"; tmp_env$f_loc <- f})
#
#     d <- data.frame(a=1)
#
#     df <- Test$new(data.table(a=1:3, b=1:3))
#
#     # LENGTH 1
#     expect_null(df$t_parse_i(NULL))
#     expect_equal(df$t_parse_i(a), quote(a))
#     expect_equal(df$t_parse_i("a"), "a")
#     expect_equal(df$t_parse_i(FALSE), FALSE)
#     expect_equal(df$t_parse_i(2), 2)
#     expect_equal(df$t_parse_i(2L), 2L)
#     expect_equal(df$t_parse_i(chr), chr)
#     expect_equal(df$t_parse_i(log), log)
#     expect_error(df$t_parse_i(blt_1))
#     expect_equal(df$t_parse_i(a==2), quote(a==2))
#     expect_error(df$t_parse_i(blt_2))
#     expect_error(df$t_parse_i(fun_base), label="Function without args")
#     expect_warning(df$t_parse_i(d))
#     suppressWarnings(expect_equal(df$t_parse_i(d), quote(get("d", envir=private$i_env))))
#
#     expect_error(df$t_parse_i(mean()))
#     # LENGTH > 1
#     expect_equal(df$t_parse_i(mean(1:5)), quote(mean(1:5)))
#     expect_equal(df$t_parse_i(list(a)), quote(list(a)))
#
#     expect_error(df$t_parse_i(a == 3 & tmp_env$f_loc(a)))
#     expect_equal(df$t_parse_i(a == 3 & b > 2), quote(a == 3 & b > 2))
#     expect_equal(df$t_parse_i(a == 3 & !b > 2 | sqrt(b) > 2), quote(a == 3 & !b > 2 | sqrt(b) > 2))
#     # expect_equal(df$t_parse_i(a != 3 & is_greater_than_2(a) | sqrt(b) > 2), quote(a != 3 & is_greater_than_2(a) | sqrt(b) > 2))
# })



