test_that("import function works for defined functions", {
    sf <- StatFrame$new(data.table(a=1:5, b=6:10))
    sum_squares <- function(x) sum(x**2)
    sf$import_function(sum_squares)
    sf$import_function(sum_squares, "ss")

    expect_equal(sf$imported_functions$sum_squares(1:2), 5)
    expect_equal(sf$imported_functions$ss(1:2), 5)
})

test_that("import function works with anonymous functions", {
    sf <- StatFrame$new(data.table(a=1:5, b=6:10))
    sf$import_function(function(x) sum(x**2), "ss")
    expect_equal(sf$imported_functions$ss(1:2), 5)
})

test_that("import function does not work with unnamed anonymous functions or badly named functions", {
    sf <- StatFrame$new(data.table(a=1:5, b=6:10))
    expect_error(sf$import_function(function(x) sum(x**2)))
    expect_error(sf$import_function(function(x) sum(x**2), "a+3"))
    expect_error(sf$import_function(function(x) sum(x**2), "a 3"))
    expect_error(sf$import_function(function(x) sum(x**2), "a>3"))
})





test_that("count returns a data.table with count of the number of rows", {
    sf <- StatFrame$new(data.table(a=1:5, b=1:5))
    expect_equal(sf$count(), data.table(N=5))

    sf <- StatFrame$new(data.frame(a=c(1,1,1,2,3), b=1:5))
    expect_equal(sf$count(.(a)), data.table(a=c(1,2,3), N=c(3,1,1), key = "a"))
    expect_equal(sf$count(list(a)), data.table(a=c(1,2,3), N=c(3,1,1), key = "a"))
    expect_equal(sf$count("a"), data.table(a=c(1,2,3), N=c(3,1,1), key = "a"))

    expect_equal(sf$count(a>2), data.table(a=c(FALSE, TRUE), N=c(4, 1), key = "a"))
    expect_equal(sf$count(.(d = a>2)), data.table(d=c(FALSE, TRUE), N=c(4, 1), key = "d"))
})



test_that("Aggregate works with grouping and filtering", {
    sf <- StatFrame$new(data.table(a=1:5, b=6:10, c = c("a", "bb", "c", "dd", "eee"), g = c(TRUE, FALSE, FALSE, TRUE, TRUE)))

    expect_equal(sf$aggregate(list(mean(x), sum(x)), columns=is.numeric), data.table(fun=c("mean", "sum"), a=c(3,15), b=c(8,40)))

    expect_equal(sf$aggregate(list(max(x), mean(x)), columns="a", by = .(cond = !b %in% c(6, 7))),
                 data.table(cond=c(FALSE, FALSE, TRUE, TRUE),
                            fun = c("max", "mean", "max", "mean"),
                            a = c(2.0, 1.5, 5.0, 4.0),
                            key = "cond")
    )

    expect_equal(sf$aggregate(list(max(x), mean(x)), columns=is.numeric, by = .(cond = !b %in% c(6, 7))),
                 data.table(cond=c(FALSE, FALSE, TRUE, TRUE),
                            fun = c("max", "mean", "max", "mean"),
                            a = c(2.0, 1.5, 5.0, 4.0),
                            b = c(7.0, 6.5, 10.0, 9.0),
                            key = "cond")
    )

    expect_equal(sf$filter(a!=2)$aggregate(list(mean(x), sum(x)), by=.(a==2), columns=.(b)),
                 data.table(
                     `a == 2`=c(FALSE, FALSE),
                     fun=c("mean", "sum"), b=c(8.25, 33), key="a == 2")
    )

    expect_equal(sf$filter(!a%in%2)$aggregate(list(mean(x), sum(x)), by=.(a%in%2), columns=is.logical),
                 data.table(
                     `a %in% 2`=c(FALSE, FALSE),
                     fun=c("mean", "sum"), g=c(0.75, 3.00), key="a %in% 2")
    )

})

test_that("Aggregate works with locally defined functions", {
    sf <- StatFrame$new(data.table(a=1:5, b=6:10))
    sum_squares <- function(x) sum(x**2)
    sf$import_function(sum_squares)
    expect_equal(sf$aggregate(list(sum_squares(x))), data.table(fun=c("sum_squares"), a=c(55), b=c(330)))
})

test_that("Aggregate currently does not work anonymous functions", {
    sf <- StatFrame$new(data.table(a=1:5, b=6:10))
    expect_error(sf$aggregate(list(mean(x), function(x) sum(x**2))))
})


test_that("map works with grouping and filtering", {
    sf <- StatFrame$new(data.table(a=1:5, b=6:10, c = c("a", "bb", "c", "dd", "eee"), g = c(TRUE, FALSE, FALSE, TRUE, TRUE)))

    expect_equal(sf$aggregate(list(mean(x), sum(x)), columns=is.numeric), data.table(fun=c("mean", "sum"), a=c(3,15), b=c(8,40)))

    expect_equal(sf$aggregate(list(max(x), mean(x)), columns="a", by = .(cond = !b %in% c(6, 7))),
                 data.table(cond=c(FALSE, FALSE, TRUE, TRUE),
                            fun = c("max", "mean", "max", "mean"),
                            a = c(2.0, 1.5, 5.0, 4.0),
                            key = "cond")
    )

    expect_equal(sf$aggregate(list(max(x), mean(x)), columns=is.numeric, by = .(cond = !b %in% c(6, 7))),
                 data.table(cond=c(FALSE, FALSE, TRUE, TRUE),
                            fun = c("max", "mean", "max", "mean"),
                            a = c(2.0, 1.5, 5.0, 4.0),
                            b = c(7.0, 6.5, 10.0, 9.0),
                            key = "cond")
    )

    expect_equal(sf$filter(a!=2)$aggregate(list(mean(x), sum(x)), by=.(a==2), columns=.(b)),
                 data.table(
                     `a == 2`=c(FALSE, FALSE),
                     fun=c("mean", "sum"), b=c(8.25, 33), key="a == 2")
    )

    expect_equal(sf$filter(!a%in%2)$aggregate(list(mean(x), sum(x)), by=.(a%in%2), columns=is.logical),
                 data.table(
                     `a %in% 2`=c(FALSE, FALSE),
                     fun=c("mean", "sum"), g=c(0.75, 3.00), key="a %in% 2")
    )

})

test_that("map works with locally defined functions", {
    sf <- StatFrame$new(data.table(a=1:5, b=6:10))
    sum_squares <- function(x) sum(x**2)
    sf$import_function(sum_squares)
    expect_equal(sf$aggregate(list(sum_squares(x))), data.table(fun=c("sum_squares"), a=c(55), b=c(330)))
})

test_that("map currently does not work anonymous functions", {
    sf <- StatFrame$new(data.table(a=1:5, b=6:10))
    expect_error(sf$aggregate(list(mean(x), function(x) sum(x**2))))
})


test_that("describe works with grouping and filtering", {
    sf <- StatFrame$new(data.table(a=1:5, b=6:10,
                                   c = c("a", "bb", "c", "dd", "eee"),
                                   d = as.factor(c("a", "bb", "c", "dd", NA)),
                                   g = c(TRUE, FALSE, FALSE, TRUE, TRUE)))
    sf$describe()
})

