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

    x <- data.table(a=c(1,1,1,2,3), b=1:5)
    df <- DataFrame$new(x)
    expect_equal(df$count(.(a)), DF(data.table(a=c(1,2,3), N=c(3,1,1), key = "a")))
    expect_equal(df$count(list(a)), DF(data.table(a=c(1,2,3), N=c(3,1,1), key = "a")))
})


test_that("columns$names returns character vector of column names", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)

    expect_equal(df$columns$names, c("a", "b"))
})

test_that("columns$rename and $reorder can be chained", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)

    df$columns$rename(toupper)$reorder("B")
    expect_equal(df$columns$names, c("B", "A"))

})

test_that("columns$rename changes column names", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    df$columns$rename(toupper)
    expect_equal(df$columns$names, c("A", "B"))

    custom_map <- function(x) ifelse(x == "A", "AA", "BB")
    df$columns$rename(custom_map)
    expect_equal(df$columns$names, c("AA", "BB"))
})

test_that("columns$rename changes column names in place", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    old_address_tbl <- address(df$unwrap())
    old_address_col <- address(df$unwrap()$b)
    df$columns$rename(toupper)
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

test_that("reorder changes row order in place", {
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

test_that("reorder does not work with keyed data", {
    x <- data.table(a=1:5, b=5:1)
    df <- DataFrame$new(x, key = "b")
    expect_error(df$reorder(b))
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

test_that("remove does not work with out of bounds integer vector", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    expect_error(df$remove(4:6))
})

test_that("remove does not work with duplicated row numbers passed", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    expect_error(df$remove(c(4, 4)))
})

test_that("remove does not work with logical vector of smaller length", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    expect_error(df$remove(c(TRUE, TRUE, FALSE)))
})

test_that("remove treats logical NA as FALSE", {
    x <- data.table(a=1:3, b=1:3)
    df <- DataFrame$new(x)
    df$remove(c(TRUE, NA, FALSE))
    expect_equal(df$unwrap(), data.table(a=2:3, b=2:3))
})

test_that("remove does not work with longer vectors", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    expect_error(df$remove(1:10))
})

test_that("remove does not work with character vector", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)

    expect_error(df$remove("a"))
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
