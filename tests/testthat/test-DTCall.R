test_that("setting arguments in DTCall works",
          {
              e <- DTCall$new(quote(tbl))
              e$set(i = quote(a==3), j = quote(.SD))

              expect_equal(e$call()$expr, quote(`[`(x= tbl, i = a == 3, j = .SD)))


          })

test_that("renaming table does not work through DTCall$set",
          {
              e <- DTCall$new(quote(tbl))
              expect_error(e$set(x=quote(y), i = quote(a==2)))
          })

test_that("removing and overriding arguments in DTCall works",
          {
              e <- DTCall$new(quote(tbl))
              e$set(i = NULL, j = NULL)
              expect_equal(e$call()$expr, quote(tbl))

              e$set(i = quote(a==3), j = quote(.SD))
              e$set(i = NULL, j = quote(`:=` (y = x)))
              expect_equal(e$call()$expr, quote(`[`(x= tbl, j = `:=`(y = x))))
          })


test_that("getting arguments in DTCall works",
          {
              e <- DTCall$new(quote(tbl))
              e$set(i = quote(a==3), j = quote(.SD))

              expect_error(e$get(c("i", "j")))

              expect_null(e$get(c("by")))

              expect_equal(e$get("i"), quote(a == 3))
          })


test_that("call returns only x (the name of the table) if neither i nor j are specified - to prevent data.table warning",
          {
              e <- DTCall$new(quote(tbl))

              e$set(by=quote(a))

              expect_equal(e$call()$expr, quote(tbl))
          })

test_that("call subset works",
          {
              e <- DTCall$new(quote(tbl))
              e$set(i = quote(a==3), j = quote(.SD), by=quote(list(a)))

              expect_equal(e$call()$expr, quote(`[`(x=tbl, i=a==3, j=.SD, by=list(a))))
              expect_equal(e$call(subset="i")$expr, quote(`[`(x=tbl, i=a==3)))
          })


test_that("by and keyby are mutually exclusive with warning upon override",
          {
              e <- DTCall$new(quote(tbl))
              e$set(i=quote(a), by=quote(list(a)))

              expect_warning(e$set(keyby=quote(list(a))))
              expect_equal(e$call()$expr, quote(`[`(x=tbl, i=a, keyby=list(a))))
              expect_warning(e$set(by=quote(list(a))))
              expect_equal(e$call()$expr, quote(`[`(x=tbl, i=a, by=list(a))))
          })




test_that("private$parse_sdcols works", {

    Test <- R6::R6Class(
        "Test",
        inherit=DTCall,
        public=list(
            t_parse_sdcols = function(e, env=parent.frame()) {
                #browser()
                private$parse_sdcols(substitute(e), env)
            }
        )
    )


    df <- Test$new()

    c <- "a"
    d <- data.frame(x=1)
    f <- function(x) "hi"
    f2 <- function(x) `==`
    expect_error(df$t_parse_sdcols(d))
    expect_equal(df$t_parse_sdcols(c), "a")
    expect_equal(df$t_parse_sdcols("a"), "a")
    expect_equal(df$t_parse_sdcols(-"a"), quote(-"a"))
    expect_equal(df$t_parse_sdcols(!"a"), quote(!"a"))
    expect_equal(df$t_parse_sdcols(1:5), quote(1:5))
    expect_equal(df$t_parse_sdcols(-1:5), quote(-1:5))
    expect_equal(df$t_parse_sdcols(!c), quote(!"a"))
    expect_equal(df$t_parse_sdcols(is.numeric), quote(is.numeric))
    expect_equal(df$t_parse_sdcols(function(x) is.numeric(x) | is.character(x)), quote(function(x) is.numeric(x) | is.character(x)))
})



test_that("On argument parser works", {

    Test <- R6::R6Class(
        "Test",
        inherit=DTCall,
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
        inherit=DTCall,
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

