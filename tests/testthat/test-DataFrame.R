test_that("deep clone creates an entirely new object and table", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)

    df_clone <- df$clone(deep=TRUE)
    df_deep_clone <- df$deep_clone()

    expect_true(address(x) == address(df$data))
    expect_true(address(x) == address(df_clone$data))
    expect_true(address(x) != address(df_deep_clone$data))
    expect_true(address(df) != address(df_deep_clone))

})

test_that("data does not copy the object", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    expect_equal(address(x), address(df$data))
})

test_that("columns$names returns character vector of column names", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)

    expect_equal(df$columns$names, c("a", "b"))
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
    old_address_tbl <- address(df$data)
    old_address_col <- address(df$data$b)
    df$columns$rename(toupper)
    expect_equal(address(df$data), old_address_tbl)
    expect_equal(address(df$data$B), old_address_col)
})

test_that("columns$reorder changes column order in place", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x, key = "b")
    old_address_tbl <- address(df$data)
    old_address_col <- address(df$data$b)
    df$columns$reorder()
    expect_equal(df$columns$names, c("b", "a"))
    expect_equal(address(df$data), old_address_tbl)
    expect_equal(address(df$data$b), old_address_col)
    df$columns$reorder(c("a", "b"))
    expect_equal(df$columns$names, c("a", "b"))
})

test_that("reorder changes row order in place", {
    x <- data.table(a=1:5, b=5:1)
    df <- DataFrame$new(x)
    old_address_tbl <- address(df$data)
    old_address_col <- address(df$data$b)
    df$reorder(b)
    expect_equal(df$data, data.table(a=5:1, b=1:5))
    expect_equal(address(df$data), old_address_tbl)
    expect_equal(address(df$data$b), old_address_col)
    df$reorder(-b)
    expect_equal(df$data, data.table(a=1:5, b=5:1))
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
    expect_equal(res$data, data.table(a=rep(1:5, 2), b=rep(1:5, 2)))
})

test_that("is_key_unique returns TRUE when key is unique", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x, key = c("a"))
    expect_true(df$is_key_unique())
})


test_that("is_key_unique returns TRUE when there is no key set", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    expect_true(df$is_key_unique())
})


test_that("apply updates specified columns", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)

    df$apply(c("a", "b"), function(x) {x**2}, b == 4)

    expect_equal(df$data[4], data.table(a=16, b=16))
})

test_that("apply mapper can use other columns", {
    x <- data.table(a=2:6, b=1:5)
    df <- DataFrame$new(x)
    mapper <- function(x) a**2
    df$apply(c("a", "b"), mapper, b == 4)
    expect_equal(df$data[4], data.table(a=25, b=25))

    x <- data.table(a=2:6, b=1:5)
    df <- DataFrame$new(x)
    mapper <- function(x) {ifelse(x > 3 , b**2, x**2)}
    df$apply(c("a", "b"), mapper)

})


test_that("apply does not copy the data", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    mapper <- function(x) a**2
    old_address_tbl <- address(df$data)
    old_address_cola <- address(df$data$a)
    old_address_col <- address(df$data$b)

    df$apply(c("a"), mapper, b == 4)

    expect_equal(address(df$data), old_address_tbl)
    expect_equal(address(df$data$a), old_address_cola)
    expect_equal(address(df$data$b), old_address_col)
})



test_that("add adds new column by reference", {
    x <- data.table(a=2:6, b=1:5)
    df <- DataFrame$new(x)
    df$add("c", function() {a**2}, b == 4)
    expect_equal(df$data[4], data.table(a=5, b=4, c=25))

    x <- data.table(a=2:6, b=1:5)
    df <- DataFrame$new(x)
    old_address_tbl <- address(df$data)
    old_address_cola <- address(df$data$a)
    old_address_col <- address(df$data$b)
    df$add("d", function() {a**2})
    expect_equal(df$data, data.table(a=2:6, b=1:5, d=(2:6)**2))
    expect_equal(address(df$data), old_address_tbl)
    expect_equal(address(df$data$a), old_address_cola)
    expect_equal(address(df$data$b), old_address_col)

})

test_that("add does not work for existing columns", {
    x <- data.table(a=2:6, b=1:5)
    df <- DataFrame$new(x)
    expect_error(df$add("a", function() {a**2}))
})



test_that("drop removes the supplied columns", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)

    df$drop("a")

    expect_equal(names(df$data), "b")
})

test_that("drop does not copy the data object or its columns", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    old_address_tbl <- address(df$data)
    old_address_col <- address(df$data$b)
    df$drop("a")

    expect_equal(address(df$data), old_address_tbl)
    expect_equal(address(df$data$b), old_address_col)
})

test_that("remove works with unevaluated expression", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    df$remove(a > 2)
    expect_equal(df$data, data.table(a=1:2, b=1:2))
})

test_that("remove works with integer vector", {
    x <- data.table(a=1:5, b=1:5)
    df <- DataFrame$new(x)
    df$remove(3:5)
    expect_equal(df$data, data.table(a=1:2, b=1:2))
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



test_that("UpdateJoin$add works", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])
    df <- DF(x)
    rel <- Relationship$new(right=y)$on(x = y)

    df$update_join(rel, columns=list(a=3, c=ifelse(i.x == 1, 3, 2), z))

    expect_equal(df$data, data.table(
        x= c(1, 2, 3),
        y= c("A", "B", "C"),
        z = c(LETTERS[6:7], NA),
        v = c(1, 2, 3),
        a = c(3, 3, 3),
        c = c(3, 2, 2)
    ))
})

test_that("UpdateJoin$add(where) works", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])
    df <- DF(x)
    rel <- Relationship$new(right=y)$on(x = y)

    df$update_join(rel, columns=list(a=3, c=ifelse(i.x == 1, 3, 2), z), where=x %in% 1:2)

    expect_equal(x, data.table(
        x= c(1, 2, 3),
        y= c("A", "B", "C"),
        z = c("F", "G","K"),
        v = c(1, 2, 3),
        a = c(3, 3, NA),
        c = c(3, 2, NA)
    ))
    expect_equal(address(x), address(df$data))
})

test_that("UpdateJoin$add(where) works with data.frame", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.frame(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])
    df <- DF(x)
    rel <- Relationship$new(right=y)$on(x = y)

    df$update_join(rel, columns=list(a=3, c=ifelse(i.x == 1, 3, 2), z), where=x %in% 1:2)

    expect_equal(x, data.table(
        x= c(1, 2, 3),
        y= c("A", "B", "C"),
        z = c("F", "G","K"),
        v = c(1, 2, 3),
        a = c(3, 3, NA),
        c = c(3, 2, NA)
    ))
    expect_equal(address(x), address(df$data))
})

test_that("UpdateJoin$add(where) works with 2 keys", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3, w = 5:7)
    y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7], w = c(5, 8))
    df <- DF(x)
    rel <- Relationship$new(right=y)$on(x = y, w)

    df$update_join(rel, columns=list(a=3, c=ifelse(i.x == 1, 3, 2), z), where=x %in% 1:2)

    expect_equal(x, data.table(
        x= c(1, 2, 3),
        y= c("A", "B", "C"),
        z = c("F", NA,"K"), # Because there is no match on x=2,w=6 it is NA
        v = c(1, 2, 3),
        w = 5:7,
        a = c(3, 3, NA),
        c = c(3, 2, NA)
    ))
    expect_equal(address(x), address(df$data))
})

test_that("UpdateJoin$add_all works", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])
    df <- DF(x)
    rel <- Relationship$new(right=y)$on(x = y)

    df$update_join(rel)

    expect_equal(df$data, data.table(
        x= c(1, 2, 3),
        y= c("A", "B", "C"),
        z = c("I", "J","K"),
        v = c(1, 2, 3),
        x_y = c("C", "D", NA),
        z_y = c("F", "G", NA)
    ))
    expect_equal(address(x), address(df$data))
})

test_that("UpdateJoin$add_all(where) works", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])
    df <- DF(x)
    rel <- Relationship$new(right=y)$on(x = y)

    df$update_join(rel, where = x == 1)

    expect_equal(df$data, data.table(
        x= c(1, 2, 3),
        y= c("A", "B", "C"),
        z = c("I", "J","K"),
        v = c(1, 2, 3),
        x_y = c("C", NA, NA),
        z_y = c("F", NA, NA)
    ))
    expect_equal(address(x), address(df$data))
})

test_that("UpdateJoin does not work with one-to-many relationship", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:5], y = c(1, 2, 2), z = LETTERS[6:8])

    df <- DF(x)
    expect_error(df$update_join(Rel(right=y)$on(x = y), a=3, c=ifelse(i.x == 1, 3, 2), z))
})


test_that("LeftJoin works", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])
    df <- DF(x)
    rel <- Relationship$new(right=y)$on(x = y)

    result <- df$left_join(rel, add = list(a=3, c=ifelse(i.x == 1, 3, 2), z, d = x))

    expect_equal(result$data, data.table(
        x= c(1, 2, 3),
        y= c("A", "B", "C"),
        z = c("I", "J","K"),
        v = c(1, 2, 3),
        z_y = c("F", "G", NA),
        a = c(3, 3, 3),
        c = c(3, 2, 2),
        d = c("C", "D", NA)
    ))
})

test_that("LeftJoin all works", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])
    df <- DF(x)
    rel <- Relationship$new(right=y)$on(x = y)

    result <- df$left_join(rel)

    expect_equal(result$data, data.table(
        x= c(1, 2, 3),
        y= c("A", "B", "C"),
        z = c("I", "J","K"),
        v = c(1, 2, 3),
        x_y = c("C", "D", NA),
        z_y = c("F", "G", NA)
    ))
})

