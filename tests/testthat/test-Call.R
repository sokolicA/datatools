preprocess_ellipsis <- function(...) substitute(list(...))


TestCall <- expose_R6_private(Call)


#https://stackoverflow.com/questions/64789235/r-testthat-use-external-package-only-in-test-file-not-in-description


test_that("tbl_env must be an environment with a 'tbl' data.table",
          {

              expect_error(TestCall$new(tbl_env=NA))
              expect_error(TestCall$new(tbl_env=1))
              expect_error(TestCall$new(tbl_env=list()))
              expect_error(TestCall$new(tbl_env="a"))
              expect_error(TestCall$new(tbl_env=data.table::data.table(a=1)))
              tbl <- data.frame(a=1)
              expect_error(TestCall$new(environment()))
              tbl <- data.table::data.table(a=1)

              expect_true(inherits(TestCall$new(environment()), "R6"))
          })

test_that("initialization without tbl_env starts with an empty call x[]",
          {
              e <- TestCall$new()
              expect_equal(e$private_expr, quote(`[`(x= x)))
          })

test_that("initialization with tbl_env starts with an empty call .__private__$tbl[]",
          {
              tbl <- data.table::data.table(a=1)
              e <- TestCall$new(tbl_env=environment())
              expect_equal(e$private_expr, quote(`[`(x= .__private__$tbl)))
          })

test_that("setting environment works",
          {
              e <- TestCall$new()
              expect_null(e$private_env)

              e$private_set_env(NULL)
              expect_null(e$private_env)

              e$private_set_env(globalenv())
              expect_identical(e$private_env, globalenv())
              e$private_set_env(globalenv())
              expect_identical(e$private_env, globalenv())
              expect_error(e$private_set_env(environment()), "Call environment can not change")

              e$private_set_env(NULL)
              expect_null(e$private_env)

          })

test_that("$assert_named works",
          {
              e <- TestCall$new()
              expect_error(e$private_assert_named())
              expect_error(e$private_assert_named(list()))
              expect_error(e$private_assert_named(NULL))
              expect_error(e$private_assert_named(NA))
              expect_error(e$private_assert_named(c(a=1, 3)))
              expect_null(e$private_assert_named(c(a=1,d=3)))
              expect_error(e$private_assert_named(list(a=3, 1)))
              expect_null(e$private_assert_named(list(a=1,d=3)))
          })



test_that("is_column returns NULL when tbl_env is not provided",
          {
              e <- TestCall$new(tbl_env=NULL)
              expect_null(e$private_is_column("a"))
              expect_null(e$private_is_column(quote(a)))
          })

test_that("is_column returns works when tbl_env is provided",
          {
              tbl <- data.table(a=1, b=2)
              e <- TestCall$new(tbl_env=environment())
              name_1 <- quote(b)
              name_2 <- "b"
              not_name_1 <- quote(c)
              not_name_2 <- "c"

              expect_true(e$private_is_column(name_1))
              expect_true(e$private_is_column(name_2))
              expect_false(e$private_is_column(not_name_1))
              expect_false(e$private_is_column(not_name_2))

              expect_false(e$private_is_column(1))
              expect_false(e$private_is_column(mean))
          })


test_that("is_function works",
          {
              e <- TestCall$new()
              assign("env", environment(), e$.__enclos_env__$private)

              f <- function(x) x

              expect_true(e$private_is_function(quote(is.numeric)))
              expect_true(e$private_is_function(is.numeric))
              expect_true(e$private_is_function(`[`))
              expect_true(e$private_is_function(quote(`[`)))

              expect_true(e$private_is_function(f))
              expect_true(e$private_is_function(quote(f)))

              expect_false(e$private_is_function(quote(a)))
              expect_false(e$private_is_function("a"))
              expect_false(e$private_is_function(NULL))
          })

test_that("parse_x does not work when tbl_env was provided",
          {
              tbl <- data.table(a=1)
              e <- TestCall$new(tbl_env=environment())
              expect_error(e$private_parse_x(quote(b)))
          })

test_that("parse_x can not set NULL",
          {
              e <- TestCall$new()
              expect_error(e$private_parse_x(NULL))
          })

test_that("parse_x works when tbl_env is not provided",
          {
              e <- TestCall$new(tbl_env=NULL)
              new_name <- quote(b)
              expect_equal(e$private_parse_x(new_name), new_name)
          })


test_that("parse_i returns atomic vectors",
          {
              e <- TestCall$new(tbl_env=NULL)
              expect_equal(e$private_parse_i(NULL), NULL)
              expect_equal(e$private_parse_i(1), 1)
              expect_equal(e$private_parse_i(1L), 1L)
              expect_equal(e$private_parse_i("a"), "a")

              tbl <- data.table(a=1)
              e <- TestCall$new(tbl_env=environment())
              expect_equal(e$private_parse_i(NULL), NULL)
              expect_equal(e$private_parse_i(1), 1)
              expect_equal(e$private_parse_i(1L), 1L)
              expect_equal(e$private_parse_i("a"), "a")

          })

test_that("parse_i returns error if function is passed (not function call)",
          {
              e <- TestCall$new(tbl_env=NULL)
              expect_error(e$private_parse_i(quote(is.integer)),
                           regexp="Can not pass functions")

              tbl <- data.table(a=1)
              e <- TestCall$new(tbl_env=environment())
              expect_error(e$private_parse_i(quote(is.integer)),
                           regexp="Can not pass functions")
          })

test_that("parse_i without tbl_env warns if object with the symbol name is found in caller environment",
          {
              e <- TestCall$new(tbl_env=NULL)
              e$private_set_env(environment())

              expect_equal(e$private_parse_i(quote(a)), quote(a))

              expect_warning(e$private_parse_i(quote(e)))
              suppressWarnings(expect_equal(e$private_parse_i(quote(e)), quote(e)))
          })


test_that("parse_i with tbl_env errors symbol name is not found a column",
          {
              tbl <- data.table(a=1)
              e <- TestCall$new(tbl_env=environment())
              expect_error(e$private_parse_i(quote(e)),
                           regexp="Only column")

          })

test_that("parse_i returns .v variables",
          {
              tbl <- data.table(a=1)
              e <- TestCall$new(tbl_env=environment())
              expect_equal(e$private_parse_i(quote(.v(a))), quote(.v(a)))
          })

test_that("parse_i returns extractions (bypasses the column name check)",
          {
              tbl <- data.table(a=1)
              e <- TestCall$new(tbl_env=environment())

              expect_equal(e$private_parse_i(quote(abc$e)), quote(abc$e))
          })

test_that("parse_i accepts function calls and abides symbol rules",
          {
              tbl <- data.table(a=1)
              e <- TestCall$new(tbl_env=environment())
              expect_error(e$private_parse_i(quote(mean(b))), "Only column")
              expect_equal(e$private_parse_i(quote(mean(a))), quote(mean(a)))
              expect_error(e$private_parse_i(quote(grepl("a", b))))
              expect_equal(e$private_parse_i(quote(grepl("a", .v(b)))), quote(grepl("a", .v(b))))
              expect_error(e$private_parse_i(quote(grepl("a", b))))
              expect_equal(e$private_parse_i(quote(grepl("a", a))), quote(grepl("a", a)))

              expect_error(e$private_parse_i(quote(a > 2 & grepl("a", b))), "Only column")
              expect_equal(e$private_parse_i(quote(a > 2 & grepl("a", a))),
                           quote(a > 2 & grepl("a", a)))

          })



test_that("parse_by works", {
    tbl <- data.table(a=1, b="b")
    e <- TestCall$new(tbl_env=environment())

    a <- "b"

    g1 <- preprocess_ellipsis(a)
    g2 <- preprocess_ellipsis(a, c=b)
    g3 <- preprocess_ellipsis(a, b, c)

    g4 <- preprocess_ellipsis("a")
    g5 <- preprocess_ellipsis("a", "b")

    g6 <- preprocess_ellipsis(a:b)
    g7 <- preprocess_ellipsis(a:c)

    g8 <- preprocess_ellipsis(c = b==.v(a))
    g9 <- preprocess_ellipsis(a, b=grepl("b", b), c=grepl(.v(a), b))
    g10 <- preprocess_ellipsis(b=grepl("b", d))


    expect_equal(e$private_parse_by(g1), quote(list(a)))
    expect_equal(e$private_parse_by(g2), quote(list(a, c=b)))
    expect_error(e$private_parse_by(g3))

    expect_equal(e$private_parse_by(g4), quote(list("a")))
    expect_equal(e$private_parse_by(g5), quote(list("a", "b")))

    expect_equal(e$private_parse_by(g6), quote(list(a:b)))
    expect_error(e$private_parse_by(g7))

    expect_equal(e$private_parse_by(g8), quote(list(c=b==.v(a))))
    expect_equal(e$private_parse_by(g9), quote(list(a, b=grepl("b", b), c=grepl(.v(a), b))))
    expect_error(e$private_parse_by(g10), "Only column")
})

test_that("parse_.SDcols returns NULL to NULL arg", {
    TestCall <- expose_R6_private(Call)
    tbl <- data.table(a=1, b="b")
    e <- TestCall$new(tbl_env=environment())

    expect_equal(e$private_parse_.SDcols(NULL), NULL)
})

test_that("parse_.SDcols works with single arg", {
    TestCall <- expose_R6_private(Call)
    tbl <- data.table(a=1, b="b")
    e <- TestCall$new(tbl_env=environment())

    expect_error(e$private_parse_.SDcols(preprocerss_ellipsis(e)))

    expect_equal(e$private_parse_.SDcols(preprocess_ellipsis(a)), "a")
    expect_equal(e$private_parse_.SDcols(preprocess_ellipsis("a")), "a")
    expect_equal(e$private_parse_.SDcols(preprocess_ellipsis(c(1,2,3))), quote(c(1,2,3)))

    expect_equal(e$private_parse_.SDcols(preprocess_ellipsis(1:5)), quote(1:5))
    expect_equal(e$private_parse_.SDcols(preprocess_ellipsis(a:b)), quote(a:b))

    expect_equal(e$private_parse_.SDcols(preprocess_ellipsis(is.numeric)), quote(is.numeric))
    expect_equal(e$private_parse_.SDcols(preprocess_ellipsis(patterns("a", "b"))), quote(patterns("a", "b")))
    expect_equal(e$private_parse_.SDcols(preprocess_ellipsis(.v(a))), quote(.v(a)))
})


test_that("parse_.SDcols_combine combines multiple args", {
    TestCall <- expose_R6_private(Call)
    tbl <- data.table(a=1, b="b", c=TRUE)
    e <- TestCall$new(tbl_env=environment())

    expect_equal(e$private_parse_.SDcols_combine(preprocess_ellipsis("a", "b")), c("a", "b"))
    expect_equal(e$private_parse_.SDcols_combine(preprocess_ellipsis(1, 2)), c("a", "b"))
    expect_equal(e$private_parse_.SDcols_combine(preprocess_ellipsis(1:2, "c")), c("a", "b", "c"))
    expect_equal(e$private_parse_.SDcols_combine(preprocess_ellipsis(a:c, !"b")), c("a", "c"))
    expect_equal(e$private_parse_.SDcols_combine(preprocess_ellipsis(a:c, -"b")), c("a", "c"))
    expect_equal(e$private_parse_.SDcols_combine(preprocess_ellipsis(1:3, -"b")), c("a", "c"))
    expect_equal(e$private_parse_.SDcols_combine(preprocess_ellipsis(1:3, -"b")), c("a", "c"))
    expect_equal(e$private_parse_.SDcols_combine(preprocess_ellipsis(is.logical, -"c")), character(0))
})





test_that("On argument parser works", {

    TestCall <- expose_R6_private(Call, debug = FALSE)
    tbl <- data.table(a=1, b="b", c=TRUE)
    e <- TestCall$new(tbl_env=environment())

    # EQUI JOINS
    expect_equal(e$private_parse_on("a"), quote(c(a = "a")))
    expect_equal(e$private_parse_on(quote(c("a", "b"))), quote(c(a="a", b="b")))
    expect_equal(e$private_parse_on(quote(c(a="b", b="a"))), quote(c(a="b", b="a")))
    expect_equal(e$private_parse_on(quote(c(a="b", "b"))), quote(c(a="b", b="b")))
    expect_equal(e$private_parse_on(quote(list(a, b))), quote(c(a="a", b="b")))
    expect_equal(e$private_parse_on(quote(.(a, b))), quote(c(a="a", b="b")))
    expect_equal(e$private_parse_on(quote(list(a=b, b=a))), quote(c(a="b", b="a")))
    expect_equal(e$private_parse_on(quote(list(a=b, b))), quote(c(a="b", b="b")))
    #e$private_parse_on(c("a==b", "b")) # maybe in the future

    # NON-EQUI JOINS
    expect_equal(e$private_parse_on(quote(list(a>b))), quote(c(a = "a>b")))
    expect_equal(e$private_parse_on(quote(list(a>b, b))), quote(c(a = "a>b", b="b")))
    expect_equal(e$private_parse_on(quote(.(a>b))), quote(c(a = "a>b")))
    expect_equal(e$private_parse_on(quote(.(a>b, b))), quote(c(a = "a>b", b="b")))
    expect_equal(e$private_parse_on(quote(c("a>b"))), quote(c(a = "a>b")))
    expect_equal(e$private_parse_on(quote(c("a>b", "b"))), quote(c(a = "a>b", b="b")))
})

test_that("reversing the on expression works", {


    TestCall <- expose_R6_private(Call)
    tbl <- data.table(a=1, b="b", c=TRUE)
    e <- TestCall$new(tbl_env=environment())

    # EQUI JOINS
    expect_equal(e$private_.reverse_on(quote(c(a = "a"))), quote(c(a = "a")))
    expect_equal(e$private_.reverse_on(quote(c(a="a", b="b"))), quote(c(a="a", b="b")))
    expect_equal(e$private_.reverse_on(quote(c(a="b", b="a"))), quote(c(b="a", a="b")))
    expect_equal(e$private_.reverse_on(quote(c(a="b", b="b"))), quote(c(b="a", b="b")))

    # NON-EQUI JOINS
    expect_equal(e$private_.reverse_on(quote(c(b = "b!=a"))), quote(c(a = "a!=b")))
    expect_equal(e$private_.reverse_on(quote(c(a = "a>b"))), quote(c(b = "b<=a")))
    expect_equal(e$private_.reverse_on(quote(c(a = "a<=b"))), quote(c(b = "b>a")))
    expect_equal(e$private_.reverse_on(quote(c(a = "a>b", b="a"))), quote(c(b = "b<=a", a="b")))
})



test_that("build_call returns without brackets if neither i nor j are set", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new(tbl_env=NULL)
    expect_equal(e$private_build_call(), quote(x))
    e$set(by="a")
    expect_equal(e$private_build_call(), quote(x))
})

test_that("build_call returns with brackets if either i or j are set", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new(tbl_env=NULL)$set(i=1)
    expect_equal(e$private_build_call(), call("[", x=quote(x), i=1))
    e <- TestCall$new(tbl_env=NULL)$set(i=NULL, j=1)
    expect_equal(e$private_build_call(), call("[", x=quote(x), j=1))
})

test_that("build_eval_env creates an environment consisting of
          the table environment and whose parent is the caller environment", {
              TestCall <- expose_R6_private(Call)
              tbl <- data.table(a=1, b="b", c=TRUE)
              e <- TestCall$new(tbl_env=environment())$set(i=1)
              eval_env <- e$private_build_eval_env(environment())

              expect_equal(parent.env(eval_env), environment())
              expect_true(exists(".v", envir = eval_env, inherits = FALSE))
              expect_true(exists(".__private__", envir = eval_env, inherits = FALSE))
          })


test_that(".v function in eval_env finds the objects from the caller environment", {
    TestCall <- expose_R6_private(Call)
    tbl_env <- new.env()
    tbl_env$tbl <- data.table(a=1:3, b="b", c=TRUE)
    tbl_env$a <- 3
    a <- 1
    e <- TestCall$new(tbl_env=tbl_env)$set(i=quote(.v(a)))
    eval_env <- e$private_build_eval_env(environment())
    eval_env$a <- 2
    expect_equal(
        eval(e$private_expr, envir=eval_env, enclos=eval_env)$a,
        1)

    e <- TestCall$new(tbl_env=tbl_env)$set(i=quote(.v(b)))
    eval_env <- e$private_build_eval_env(environment())
    eval_env$b <- 2
    expect_error(eval(e$private_expr, envir=eval_env, enclos=eval_env),
                 "'b' not found")
})



test_that("eval evaluates and resets call after evaluating", {
    TestCall <- expose_R6_private(Call)
    tbl <- data.table(a=1:3, b="b", c=TRUE)
    e <- TestCall$new(tbl_env=environment())$set(i=2)
    result <- e$eval(environment())
    print(tbl[2])
    expect_equal(result, tbl[2])
    expect_equal(e$private_expr, call("[", x=quote(.__private__$tbl)))
    expect_null(e$private_env)
})



test_that("reset keeps the table name and removes the env", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new()$set(i=1)
    e$private_reset()
    expect_equal(e$private_expr, call("[", x=quote(x)))
    expect_null(e$private_env)
})



test_that("subset subsets the call", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new()$set(i=1, j=1, by="a", on="a")
    call <- e$private_expr
    e$subset(c("i", "j", "by", "on", "keyby"))
    expect_equal(call, e$private_expr)

    e$subset(c("j", "by"))
    expect_equal(e$private_expr, call("[", x=quote(x), j=1, by="a"))

    e$subset(NULL)
    expect_equal(e$private_expr, call("[", x=quote(x)))
    expect_null(e$private_env)
})



test_that("arg gets a specific argument", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new()$set(i=1, j=quote(.SD), by=quote(list(a)), on="a")

    expect_equal(e$arg("i"), 1)
    expect_equal(e$arg("j"), quote(.SD))
    expect_equal(e$arg("j", as_chr=TRUE), ".SD")
    expect_equal(e$arg("by", as_chr=FALSE), quote(list(a)))
    expect_equal(e$arg("by", as_chr=TRUE), "a")
})


test_that("arg returns null if no arg found", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new()$set(i=1, j=1, by="a", on="a")

    expect_null(e$arg(".SDcols"))
    expect_null(e$arg(".SDcols", as_chr=TRUE))
})


test_that("grouping returns null if by and keyby are not set", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new()$set(i=1, j=1, on="a")

    expect_null(e$grouping())
    expect_null(e$grouping(as_chr=TRUE))
})

test_that("grouping returns grouping when either by or keyby are set", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new()$set(i=1, by="a")

    expect_equal(e$grouping(), "a")

    e <- TestCall$new()$set(by=NULL, keyby="b")
    expect_equal(e$grouping(), "b")
})



test_that("$selected_columns does not work if tbl_env not set", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new()$set(i=1, by="a")
    expect_error(e$selected_columns(), "Can not evaluate without tbl_env")
})

test_that("$selected_columns returns the column names selected with .SDcols", {
    TestCall <- expose_R6_private(Call)
    tbl <- data.table(a=1L, b="b", c=TRUE)
    e <- TestCall$new(tbl_env=environment())$set(i=NULL, .SDcols=quote(list("a")), env=environment())
    expect_equal(e$selected_columns(environment()), "a")
    e <- TestCall$new(tbl_env=environment())$set(i=NULL, .SDcols=quote(list(!is.integer)), env=environment())
    expect_equal(e$selected_columns(environment()), c("b", "c"))
})


test_that("$find_parser throws an error if parser is not found!", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new()

    expect_error(e$private_find_parser("b"), "Can not set")
    expect_error(e$private_find_parser(""), "Can not set")
})

test_that("$find_parser finds correct parser for arg", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new()
    expect_equal(deparse(e$private_find_parser("x")), deparse(e$private_parse_x))
    expect_equal(deparse(e$private_find_parser("i")), deparse(e$private_parse_i))
})

test_that("$set_handle_by removes the conflicting by/keyby setting", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new()
    e$set(by=quote(list("a")))
    e$private_set_handle_by("by")
    expect_equal(e$private_expr[["by"]], quote(list("a")))
    expect_message(e$private_set_handle_by("keyby"))
    expect_null(e$private_expr[["by"]])

    e$set(keyby=quote(list("a")))
    e$private_set_handle_by("keyby")
    expect_equal(e$private_expr[["keyby"]], quote(list("a")))
    expect_message(e$private_set_handle_by("by"))
    expect_null(e$private_expr[["keyby"]])
})

test_that("$add_parsed does nothing if NULL is passed and arg is not set", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new()
    e$private_add_parsed(list(i=NULL))
    expect_equal(e$private_expr, call("[", x=quote(x)))
})

test_that("$add_parsed removes arg if NULL is passed and arg is not set", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new()
    e$set(i=quote(.v(a) >1))
    e$private_add_parsed(list(i=NULL))
    expect_equal(e$private_expr, call("[", x=quote(x)))
})

test_that("$add_parsed adds multiple arguments", {
    TestCall <- expose_R6_private(Call)
    e <- TestCall$new()
    e$set(i=quote(.v(a) >1))
    e$private_add_parsed(list(i=NULL, by = quote(list("a")), j = quote(.SD)))
    expect_null(e$private_expr[["i"]])
    expect_true(!is.null(e$private_expr[["by"]]))
    expect_true(!is.null(e$private_expr[["j"]]))
})

test_that("$set works", {
    e <- Call$new()
    e$set(i=1, j=quote(.SD))
    expect_equal(e$get(), call("[", x=quote(x), i=1, j=quote(.SD)))
    e$set(.SDcols=quote(list("a")))
    expect_equal(e$get(), call("[", x=quote(x), i=1, j=quote(.SD), .SDcols="a"))
    e$set(i=2, j=NULL, .SDcols=NULL)
    expect_equal(e$get(), call("[", x=quote(x), i=2))
})


