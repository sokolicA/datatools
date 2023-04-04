test_that("deep clone creates an entirely new object and table", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    sf <- StatFrame$new(df)

    sf_clone <- sf$clone(deep=TRUE)
    sf_deep_clone <- sf$deep_clone()

    expect_true(address(x) == address(df$data))
    expect_true(address(x) == address(sf_clone$data))
    expect_true(address(x) != address(sf_deep_clone$data))
    expect_true(address(sf) != address(sf_deep_clone))

})

test_that("group creates a shallow clone of the object with group_by specified", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    sf <- StatFrame$new(df)

    sfg <- sf$group("a")

    expect_true(address(x) == address(sfg$data))
    expect_false(sf$is_grouped())
    expect_true(sfg$is_grouped())
})


test_that("count works", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    sf <- StatFrame$new(df)

    expect_equal(sf$count(), data.table(N = 5))

    sfg <- sf$group("a")
    expect_equal(sfg$count(), data.table(a=1:5, N = 1, key = "a"))

})



