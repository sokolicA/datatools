test_that("deep clone creates an entirely new object and table", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)

    df_shallow_clone <- df$clone(deep=FALSE)
    df_deep_clone <- df$clone(deep=TRUE)

    expect_true(address(x) == address(df$unwrap()))
    expect_true(address(x) == address(df_shallow_clone$unwrap()))
    expect_true(address(x) != address(df_deep_clone$unwrap()))
    expect_true(address(df) != address(df_deep_clone))

})

test_that("data does not copy the object", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    expect_equal(address(x), address(df$unwrap()))
})

test_that("count returns a DataFrame with count of the number of rows", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    expect_equal(df$count(), DF(data.table(N=5)))

    df <- DF(data.frame(x=1:5, g = c("a", "a", "b", "c", "c")))
    expect_equal(df$count(), DF(data.table(N=5)))
    expect_equal(df$group_by(g)$count(), DF(data.table(g=c("a", "b", "c"), N=c(2,1,2)), key="g"))
})


test_that("columns$names returns character vector of column names", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)

    expect_equal(df$columns$names, c("a", "b"))
})

test_that("columns$rename_with and $reorder can be chained", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)

    df$columns$rename_with(toupper)$reorder("B")
    expect_equal(df$columns$names, c("B", "A"))

})

test_that("columns$rename changes column names", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    df$columns$rename(c("a" = "A", "b"="B"))
    expect_equal(df$columns$names, c("A", "B"))
    df$columns$rename(c("A" = "b"))
    expect_equal(df$columns$names, c("b", "B"))
})

test_that("columns$rename_with changes column names", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    df$columns$rename_with(toupper)
    expect_equal(df$columns$names, c("A", "B"))

    custom_map <- function(x) ifelse(x == "A", "AA", "BB")
    df$columns$rename_with(custom_map)
    expect_equal(df$columns$names, c("AA", "BB"))
})

test_that("columns$rename_with changes column names in place", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    old_address_tbl <- address(df$unwrap())
    old_address_col <- address(df$unwrap()$b)
    df$columns$rename_with(toupper)
    expect_equal(address(df$unwrap()), old_address_tbl)
    expect_equal(address(df$unwrap()$B), old_address_col)
})

test_that("columns$reorder changes column order in place", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x, key = "b")
    old_address_tbl <- address(df$unwrap())
    old_address_col <- address(df$unwrap()$b)
    df$columns$reorder()
    expect_equal(df$columns$names, c("b", "a"))
    expect_equal(address(df$unwrap()), old_address_tbl)
    expect_equal(address(df$unwrap()$b), old_address_col)
    df$columns$reorder(c("a", "b"))
    expect_equal(df$columns$names, c("a", "b"))
})

test_that("sort changes row order in place", {
    x <- data.table(a=1:5, b=5:1)
    df <- DataFrame$new(x)
    old_address_tbl <- address(df$unwrap())
    old_address_col <- address(df$unwrap()$b)
    df$sort(b)
    expect_equal(df$unwrap(), data.table(a=5:1, b=1:5))
    expect_equal(address(df$unwrap()), old_address_tbl)
    expect_equal(address(df$unwrap()$b), old_address_col)
    df$sort(-b)
    expect_equal(df$unwrap(), data.table(a=1:5, b=5:1))
})

test_that("sort does not work with keyed data", {
    x <- data.table(a=1:5, b=5:1)
    df <- DataFrame$new(x, key = "b")
    expect_error(df$sort(b))
})


test_that("append appends the table if data.frame is passed", {
    x <- data.table(a=1:5, b=1:5)
    y <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    res <- df$append(y)
    expect_equal(res$unwrap(), data.table(a=rep(1:5, 2), b=rep(1:5, 2)))
})

test_that("is_key_unique returns TRUE when key is unique", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x, key = c("a"))
    expect_true(df$is_key_unique())
})


test_that("is_key_unique returns FALSE when there is no key set", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    expect_false(df$is_key_unique())
})



test_that("drop removes the supplied columns", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)

    df$columns$drop("a")

    expect_equal(names(df$unwrap()), "b")
})

test_that("drop does not copy the data object or its columns", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    old_address_tbl <- address(df$unwrap())
    old_address_col <- address(df$unwrap()$b)
    df$columns$drop("a")

    expect_equal(address(df$unwrap()), old_address_tbl)
    expect_equal(address(df$unwrap()$b), old_address_col)
})

test_that("remove works with unevaluated expression", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    df$remove(a > 2)
    expect_equal(df$unwrap(), data.table(a=1:2, b=1:2))
})

test_that("remove works with integer vector", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    df$remove(3:5)
    expect_equal(df$unwrap(), data.table(a=1:2, b=1:2))
})

test_that("remove does work with out of bounds integer vector", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    df$remove(4:6)
    expect_equal(df$unwrap(), data.table(a=1:3, b=1:3))
})

test_that("remove does work with duplicated row numbers passed", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    df$remove(c(4, 4))
    expect_equal(df$unwrap(), data.table(a=c(1:3, 5),  b=c(1:3, 5)))
})

# test_that("remove works with logical vector of smaller length and will repeat the vector...", {
#     x <- data.table(a=1:5, b=1:5)
#     df <- DataFrame$new(x)
#     expect_error(df$remove(c(TRUE, TRUE, FALSE)))
# })

test_that("remove treats logical NA as FALSE", {
    x <- data.table(a=1:3, b=1:3)
    df <- DataFrame$new(x)
    df$remove(c(TRUE, NA, FALSE))
    expect_equal(df$unwrap(), data.table(a=2:3, b=2:3))
})

test_that("remove works with longer vectors", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    df$remove(1:10)
    expect_equal(df$unwrap(), x[0])
})

test_that("remove does not work with character vector", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    df$remove("a")
    expect_equal(df, DataFrame$new(x))
})


test_that("filter works with unevaluated expression", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    filt_df <- df$filter(a > 2)
    expect_equal(filt_df$unwrap(), data.table(a=3:5, b=3:5))
})

test_that("filter works with integer vector", {
    expect_equal(DataFrame$new(data.table(a=1:5, b=1:5))$filter(3:5)$unwrap(),
                 data.table(a=3:5, b=3:5))
})

test_that("filter does not work with logical vector of smaller length", {
    expect_error(DataFrame$new(data.table(a=1:5, b=1:5))$filter(c(TRUE, TRUE, FALSE)))
})

test_that("filter treats logical NA as FALSE", {
    df <- DataFrame$new(data.table(a=1:3, b=1:3))
    expect_equal(df$filter(c(TRUE, NA, FALSE))$unwrap(), data.table(a=1, b=1))
})

test_that("filter does not work with character vector", {
    expect_error(DataFrame$new(data.table(a=1:5, b=1:5))$filter("a"))
})


TestPrivateDF <- R6::R6Class(
    "TestPrivateDF",
    inherit=DataFrame,
    public=list(
        t_by = function() return(private$by),
        t_parse_i = function(e, env=parent.frame()) {
            private$parse_expr(substitute(e), env)
        }
    )
)

TestPrivateCol <- R6::R6Class(
    "TestPrivateCol",
    inherit=Columns,
    public=list(
        t_rename_grouping = function(e, old, new) private$rename_grouping(e, old, new),
        t_find_group_cols = function(e) private$find_group_cols(e)
    )
)

test_that("group_by works", {
    x <- DataFrame$new(data.frame(x=1:5, g = c("a", "a", "b", "c", "c")))
    expect_error(x$group_by(c))
    expect_error(x$group_by(1))
    expect_error(x$group_by("c"))
    x <- TestPrivateDF$new(data.frame(x=1:5, g = c("a", "a", "b", "c", "c")))
    x$group_by(x)
    expect_equal(x$t_by(), quote(list(x)))
    x$group_by(NULL)
    expect_null(x$t_by())
    x$group_by("x")
    expect_equal(x$t_by(), quote(list(x)))
    x$group_by(x, x >2, grepl("a", g))
    expect_equal(x$t_by(), quote(list(x, x>2, grepl("a", g))))

    grping <- "x"
    expect_error(df$group_by(grping))
    x$group_by(c(grping))
    expect_equal(x$t_by(), quote(list(x)))
})

test_that("find_group_col works", {
    x <- TestPrivateCol$new(data.frame(a=1))
    expect_null(x$t_find_group_cols(NULL))
    expect_equal(x$t_find_group_cols(quote(list(a, a>2, g=a !=b, d=a, a=gg, grepl("x", f, d, "g", FALSE)))),
                 c("a", "a", "a", "b", "a", "gg", "f", "d"))
})

test_that("rename_grouping works", {
    x <- TestPrivateCol$new(data.frame(a=1))
    e <- quote(list(a, a>2, g=a !=b, d=a, a=gg, f=grepl("x", a)))
    expect_equal(x$t_rename_grouping(e, c("a", "b"), c("c", "d")), quote(list(c, c>2, g=c !=d, d=c, a=gg, f=grepl("x", c))))
})


#
# test_that("subset works", {
#     df <- DataFrame$new(data.table(a=1:3, b=1:3))
#     expect_equal(df$subset(a==1)$count()$unwrap(), data.table(N=1))
#     expect_equal(df$count()$unwrap(), data.table(N=3))
#
#     expect_equal(df$subset(a>1, persist=TRUE)$count()$unwrap(), data.table(N=2))
#     expect_equal(df$count()$unwrap(), data.table(N=2))
#
#
# })



test_that("private$i works", {

    TestPrivateDF <- R6::R6Class(
        "TestPrivateDF",
        inherit=DataFrame,
        public=list(
            t_parse_i = function(e, env=parent.frame()) {
                browser()
                private$parse_i(substitute(e), env)
            }
        )
    )

    chr <- c("a", "b")
    log <- c(TRUE, FALSE)
    lng <- quote(mean)
    blt_1 <- `==`
    blt_2 <- c
    fun_base <- mean
    fun_loc <- function(x) "hi"
    df <- data.frame(a=1)

    df <- TestPrivateDF$new(data.table(a=1:3, b=1:3))

    # LENGTH 1
    expect_null(df$t_parse_i(NULL))
    expect_equal(df$t_parse_i(a), quote(a))
    expect_equal(df$t_parse_i("a"), "a")
    expect_equal(df$t_parse_i(FALSE), FALSE)
    expect_equal(df$t_parse_i(2), 2)
    expect_equal(df$t_parse_i(2L), 2L)
    expect_equal(df$t_parse_i(chr), chr)
    expect_equal(df$t_parse_i(log), log)
    expect_equal(df$t_parse_i(blt_1), quote(`==`))
    expect_equal(df$t_parse_i(blt_2), quote(c))
    expect_equal(df$t_parse_i(fun_base), quote(get("fun_base", envir=private$i_env)))
    expect_equal(df$t_parse_i(fun_loc), quote(get("fun_loc", envir=private$i_env)))
    expect_equal(df$t_parse_i(df), quote(get("df", envir=private$i_env)))

    expect_error(df$t_parse_i(mean()))
    # LENGTH > 1
    expect_equal(df$t_parse_i(mean(1:5)), quote(get("mean", envir = private$i_env)(1:5)))
    expect_equal(df$t_parse_i(list(a)), quote(list(a)))
})


test_that("private$i works 2", {

    TestPrivateDF <- R6::R6Class(
        "TestPrivateDF",
        inherit=DataFrame,
        public=list(
            t_parse_i = function(e, env=parent.frame()) {
                #browser()
                private$parse_i(substitute(e), env)
            }
        )
    )


    df <- TestPrivateDF$new(data.table(a=1:3, b=1:3))

    c <- "g"
    d <- data.frame(x=1)
    f <- function(x) "hi"
    f2 <- function(x) `==`
    expect_equal(df$t_parse_i(`==`), quote(`==`))
    expect_equal(df$t_parse_i(f2), quote(get("f2", envir = private$i_env)))
    expect_equal(df$t_parse_i(a == 3 & b > 2), quote(a == 3 & b > 2))
})



test_that("private$parse_sdcols works", {

    TestPrivateDF <- R6::R6Class(
        "TestPrivateDF",
        inherit=DataFrame,
        public=list(
            t_parse_sdcols = function(e, env=parent.frame()) {
                browser()
                private$parse_sdcols(substitute(e), env)
            }
        )
    )


    df <- TestPrivateDF$new(data.table(a=1:3, b=1:3))

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
    expect_equal(df$t_parse_sdcols(is.numeric), is.numeric)
    expect_equal(df$t_parse_sdcols(function(x) is.numeric(x) | is.character(x)), function(x) is.numeric(x) | is.character(x))
})



test_that("$set works", {

    df <- DataFrame$new(data.table(a=1:3, b=1:3, d = LETTERS[1:3]))
    df$select(is.character)$where(a==2)$set(a)
    expect_equal(df$unwrap(), data.table(data.table(a=1:3, b=1:3, d = c("A", "2", "C"))))

    df$where(b > 1)$set(fifelse(a==3, 1, 0))
    expect_equal(df$unwrap(), data.table(data.table(a=c(1, 0, 1), b=c(1, 0, 1), d = c("A", "0", "1"))))

    df$select(is.numeric)$set(NA);
    expect_equal(df$unwrap(), data.table(data.table(a=NA_integer_, b=NA_integer_, d = c("A", "0", "1"))))

    df$set(NULL) # is this ok?
    expect_equal(df$unwrap(), data.table())
})


test_that("insert works", {
    df <- DataFrame$new(data.table(a=1:3, b=1:3))

    expect_error(df$insert(a=3, g=a, b=NULL))
    expect_equal(df$insert(c=a, g=a+b)$unwrap(), data.table(a=1:3, b=1:3, c=1:3, g=2*1:3))

    df <- DataFrame$new(data.table(a=1:3, b=1:3))
    expect_equal(df$subset(a>1)$insert(c=2*a)$unwrap(), data.table(a=1:3, b=1:3, c=c(NA, 4, 6)))

    expect_equal(df$count()$unwrap(), data.table(N=3))

    expect_equal(df$subset(a==1, persist=TRUE)$count()$unwrap(), data.table(N=1))
    expect_equal(df$count()$unwrap(), data.table(N=1))

})

