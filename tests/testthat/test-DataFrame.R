test_that("$new does not copy the data.table object by default ", {
    x <- data.table(a=1)
    df <- DataFrame$new(x)
    expect_equal(address(x), address(df$unwrap()))
})

test_that("$new copies dataframes and tibbles even with copy=FALSE", {
    dataframe <- data.frame(a=1)
    tibble <- dplyr::tibble(a=1)
    expect_false(address(DF(tibble)$unwrap())==address(tibble))
    expect_false(address(DF(dataframe)$unwrap())==address(dataframe))
})

test_that("$new copies the table object if copy=TRUE", {
    dataframe <- data.frame(a=1)
    datatable <- data.table(a=1)
    tibble <- dplyr::tibble(a=1)

    expect_false(address(DF(dataframe, copy=TRUE)$unwrap())==address(dataframe))
    expect_false(address(DF(tibble, copy=TRUE)$unwrap())==address(tibble))
    expect_false(address(DF(datatable, copy=TRUE)$unwrap())==address(datatable))
})

test_that("$unwrap returns the underlying table", {
    x <- data.frame(a=1)
    df <- DataFrame$new(x)

    expect_equal(df$unwrap(), x)
})


test_that("copy creates a new DataFrame with a copied table and new empty call", {
    x <- data.table(a=1)
    df <- DataFrame$new(x)

    df_copy <- df$copy()

    expect_true(address(x) == address(df$unwrap()))
    expect_true(address(x) != address(df_copy$unwrap()))
    expect_true(address(df) != address(df_copy))

    df_call <- .subset2(df, ".__enclos_env__")$private$call
    df_copy_call <- .subset2(df_copy, ".__enclos_env__")$private$call
    expect_true(address(df_call) != address(df_copy_call))
})

test_that("count returns a DataFrame with count of the number of rows", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    expect_equal(df$count(), DF(data.table(N=5)))

    df <- DF(data.frame(x=1:5, g = c("a", "a", "b", "c", "c")))
    expect_equal(df$count(), DF(data.table(N=5)))
    expect_equal(df$group_by(g, .as_key=TRUE)$count()$unwrap(), data.table(g=c("a", "b", "c"), N=c(2,1,2), key="g"))
})


test_that("columns returns character vector of column names", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)

    expect_equal(df$columns, c("a", "b"))
})

test_that("rename_with and reorder can be chained", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)

    df$rename_with(toupper)$set_order("B")
    expect_equal(df$columns, c("B", "A"))

})

test_that("rename changes column names", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    df$rename(c("a" = "A", "b"="B"))
    expect_equal(df$columns, c("A", "B"))
    df$rename(c("A" = "b"))
    expect_equal(df$columns, c("b", "B"))
})

test_that("rename_with changes column names", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    df$rename_with(toupper)
    expect_equal(df$columns, c("A", "B"))

    custom_map <- function(x) ifelse(x == "A", "AA", "BB")
    df$rename_with(custom_map)
    expect_equal(df$columns, c("AA", "BB"))
})

test_that("rename_with changes column names in place", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    old_address_tbl <- address(df$unwrap())
    old_address_col <- address(df$unwrap()$b)
    df$rename_with(toupper)
    expect_equal(address(df$unwrap()), old_address_tbl)
    expect_equal(address(df$unwrap()$B), old_address_col)
})

test_that("reorder changes column order in place", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)$set_key("b")
    old_address_tbl <- address(df$unwrap())
    old_address_col <- address(df$unwrap()$b)
    df$set_order()
    expect_equal(df$columns, c("b", "a"))
    expect_equal(address(df$unwrap()), old_address_tbl)
    expect_equal(address(df$unwrap()$b), old_address_col)
    df$set_order(c("a", "b"))
    expect_equal(df$columns, c("a", "b"))
})

test_that("order_by changes row order in place", {
    x <- data.table(a=1:5, b=5:1)
    df <- DataFrame$new(x)
    old_address_tbl <- address(df$unwrap())
    old_address_col <- address(df$unwrap()$b)
    df$order_by(b)
    expect_equal(df$unwrap(), data.table(a=5:1, b=1:5))
    expect_equal(address(df$unwrap()), old_address_tbl)
    expect_equal(address(df$unwrap()$b), old_address_col)
    df$order_by(-b)
    expect_equal(df$unwrap(), data.table(a=1:5, b=5:1))
})

test_that("order_by does not work with keyed data", {
    x <- data.table(a=1:5, b=5:1)
    df <- DataFrame$new(x)$set_key("b")
    expect_error(df$order_by(b))
})


test_that("concat appends the table if data.frame is passed", {
    x <- data.table(a=1:5, b=1:5)
    y <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    res <- df$concat(y)
    expect_equal(res$unwrap(), data.table(a=rep(1:5, 2), b=rep(1:5, 2)))
})

test_that("is_key_unique returns TRUE when key is unique", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)$set_key("a")
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

    df$drop("a")

    expect_equal(df$columns, "b")
})

test_that("drop does not copy the data object or its columns", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    old_address_tbl <- address(df$unwrap())
    old_address_col <- address(df$unwrap()$b)
    df$drop("a")

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




test_that("find_group_col works", {
    TestPrivateCol <- R6::R6Class(
        "TestPrivateCol",
        inherit=DataFrame,
        public=list(
            t_rename_grouping = function(e, old, new) private$rename_grouping(e, old, new),
            t_find_group_cols = function(e) private$find_group_cols(e)
        )
    )


    x <- TestPrivateCol$new(data.frame(a=1))
    expect_null(x$t_find_group_cols(NULL))
    expect_equal(x$t_find_group_cols(quote(list(a, a>2, g=a !=b, d=a, a=gg, grepl("x", f, d, "g", FALSE)))),
                 c("a", "a", "a", "b", "a", "gg", "f", "d"))
})

test_that("rename_grouping works", {
    TestPrivateCol <- R6::R6Class(
        "TestPrivateCol",
        inherit=DataFrame,
        public=list(
            t_rename_grouping = function(e, old, new) private$rename_grouping(e, old, new),
            t_find_group_cols = function(e) private$find_group_cols(e)
        )
    )

    x <- TestPrivateCol$new(data.frame(a=1))
    e <- quote(list(a, a>2, g=a !=b, d=a, a=gg, f=grepl("x", a)))
    expect_equal(x$t_rename_grouping(e, c("a", "b"), c("c", "d")), quote(list(c, c>2, g=c !=d, d=c, a=gg, f=grepl("x", c))))
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
})

test_that("insert works with subset/filter on columns (J)", {
    # Taken from https://stackoverflow.com/questions/19847121/using-data-table-to-aggregate
    # when you use the first argument of [, you are subsetting; the operations in the second argument are only done for the subsetted data.table.
    # -> to insert values on all rows you should subset on J using column[...]
    df <- DF(data.table(plate = paste0("plate",rep(1:2,each=5)),
                        id = rep(c("CTRL","CTRL","ID1","ID2","ID3"),2),
                        val = 1:10))

    expected <- data.table(plate = paste0("plate",rep(1:2,each=5)),
                           id = rep(c("CTRL","CTRL","ID1","ID2","ID3"),2),
                           val = 1:10,
                           test_1 = c(rep(1.5, 5), rep(6.5, 5)))
    df$group_by(plate)$insert(test_1=mean(val[id=="CTRL"]))
    expect_equal(df$unwrap(), expected)

    expected[, test_2 := 4L]

    df$insert(test_2=mean(val[id=="CTRL"]))
    expect_equal(df$unwrap(), expected)
})



test_that("update join does not work for 1-many relationships", {

    x <- data.table(a=1:3, b = c("a", "b", "a"))
    y <- data.table(a=c("b", "b", "a"), b = 5:7)

    df <- DF(x)
    expect_error(df$update_join(y, .(b=a), insert=.(b)))
})


test_that("update join add all columns works", {

    x <- data.table(a=1:3, b = c("a", "b", "a"))
    y <- data.table(a=c("b", "c", "a"), b = 5:7)
    z <- data.table(a=c("c", "a", "d"), b = 5:7, d=1)

    df <- DF(x)
    df$update_join(y, .(b=a), insert="all")
    expect_equal(df$unwrap(), data.table(a=1:3, b = c("a", "b", "a"), b_y = c(7, 5, 7)))
    df$update_join(z, .(b=a), insert="all")
    expect_equal(df$unwrap(), data.table(a=1:3, b = c("a", "b", "a"), b_y = c(7, 5, 7), b_y_y = c(6, NA, 6), d=c(1, NA, 1)))
})


test_that("update join insert specific columns works", {

    x <- data.table(a=1:3, b = c("a", "b", "a"))
    y <- data.table(a=c("b", "c", "a"), b = 5:7)
    z <- data.table(a=c("c", "a", "d"), b = 5:7, d=1)

    df <- DF(x)
    expect_error(df$update_join(y, .(b=a), insert = .(b)))
    df$update_join(y, .(b=a), insert = .(b_y = b))
    expect_equal(df$unwrap(), data.table(a=1:3, b = c("a", "b", "a"), b_y = c(7, 5, 7)))
})

test_that("update join update specific columns works", {

    x <- data.table(a=1:3, b = c("a", "b", "a"))
    y <- data.table(a=c("b", "c", "a"), b = 5:7)
    z <- data.table(a=c("c", "a", "d"), b = 5:7, d=1)

    df <- DF(x)
    expect_error(df$update_join(y, .(b=a), update = .(g = b)))

    df$where(a<3)$update_join(y, .(b=a), insert="all", update = .(a=1))
    expect_equal(df$unwrap(), data.table(a=c(1,1,3), b = c("a", "b", "a"), b_y = c(7L, 5L, NA)))
})


test_that("update join takes correct columns from left and right table", {

    x <- data.table(a=1:3, b = c("a", "b", "a"))
    y <- data.table(a=c("b", "c", "a"), b = 5:7, d = 1)

    df <- DF(x)
    expect_error(df$update_join(y, .(b=a), insert = .(bx = i.b, d=i.d)))

    df$update_join(y, .(b=a), insert = .(bx = i.b, by=b, by2=x.b))
    expect_equal(df$unwrap(), data.table(a=1:3, b = c("a", "b", "a"),
                                         bx= c("a", "b", "a"),
                                         by = c(7,5,7), by2=c(7,5,7)))
})


test_that("left_join correctly renames and reorders columns", {

    x <- data.table(a=1:3, b = c("a", "b", "a"), c = "left", d = "left")
    y <- data.table(a=1:3, c =c("a", "b", "a"), b = "right", d = "right", e = "right")

    df <- DF(x)

    expect_equal(
        df$left_join(y, .(a, b=c))$unwrap(),
        data.table(a=1:3, b = c("a", "b", "a"), c = "left", d = "left",
                   b_y = "right", d_y = "right", e = "right")
    )
})

test_that("Aggregate works with locally defined functions", {
    df <- DF(data.table(a=1:5, b=6:10))
    sum_squares <- function(x) sum(x**2)
    expect_equal(df$aggregate(sum_squares(x))$unwrap(), data.table(fun=c("sum_squares"), a=c(55), b=c(330)))
})


test_that("Aggregate works with grouping and filtering", {
    df <- DataFrame$new(data.table(a=1:5, b=6:10, c = c("a", "bb", "c", "dd", "eee"), g = c(TRUE, FALSE, FALSE, TRUE, TRUE)))

    expect_equal(df$select(is.numeric)$aggregate(mean(x), sum(x))$unwrap(), data.table(fun=c("mean", "sum"), a=c(3,15), b=c(8,40)))

    expect_equal(df$select("a")$group_by(cond = !b %in% c(6, 7), .as_key=TRUE)$aggregate(max(x), mean(x))$unwrap(),
                 data.table(cond=c(FALSE, FALSE, TRUE, TRUE),
                            fun = c("max", "mean", "max", "mean"),
                            a = c(2.0, 1.5, 5.0, 4.0),
                            key = "cond")
    )

    expect_equal(df$select(is.numeric)$group_by(cond = !b %in% c(6, 7), .as_key=TRUE)$aggregate(max(x), mean(x))$unwrap(),
                 data.table(cond=c(FALSE, FALSE, TRUE, TRUE),
                            fun = c("max", "mean", "max", "mean"),
                            a = c(2.0, 1.5, 5.0, 4.0),
                            b = c(7.0, 6.5, 10.0, 9.0),
                            key = "cond")
    )

    expect_equal(df$where(a!=2)$group_by(a==2, .as_key=TRUE)$select("b")$aggregate(mean(x), sum(x))$unwrap(),
                 data.table(
                     `a == 2`=c(FALSE, FALSE),
                     fun=c("mean", "sum"), b=c(8.25, 33), key="a == 2")
    )

    expect_equal(df$where(!a%in%2)$group_by(a%in%2, .as_key=TRUE)$select(is.logical)$aggregate(mean(x), sum(x))$unwrap(),
                 data.table(
                     `a %in% 2`=c(FALSE, FALSE),
                     fun=c("mean", "sum"), g=c(0.75, 3.00), key="a %in% 2")
    )

})




test_that("Aggregate currently does not work anonymous functions", {
    df <- DataFrame$new(data.table(a=1:5, b=6:10))
    expect_error(df$aggregate(mean(x), function(x) sum(x**2)))
})


test_that("transform works with anonymous functions", {
    df <- DF(data.frame(a=1:5, b=1:5, c=c(1:4, NA)))
    df$where(b>3)$select("a")$transform(function(x) x + 50)
    expect_equal(df$unwrap()$a, c(1,2,3,54,55))
})

test_that("transform passes ... to fun", {
    df <- DF(data.frame(a=1:5, b=1:5, c=c(1:4, NA)))
    df$where(b>3)$select("c")$transform(mean, na.rm=T)
    expect_equal(df$unwrap()$c, c(1,2,3,4,4))
})






