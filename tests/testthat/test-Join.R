test_that("LeftJoin$add works", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:5], y = c(1, 2, 2), z = LETTERS[6:8])

    rel <- Relationship$new(left = x, right=y)$on(x = y)
    join <- LeftJoin$new(rel)
    result_1 <- join$add(columns = list(a=3, c=ifelse(x == "C", 3, 2), z))
    result_2 <- join$add(columns = list(a=3, c=ifelse(i.x == 1, 3, 2), z))

    expect_equal(result_1, data.table(
        x= c(1, 2, 2, 3),
        y= c("A", "B", "B", "C"),
        z = c("I", "J", "J", "K"),
        v = c(1, 2, 2, 3),
        z_y = c("F", "G", "H", NA_character_),
        a = c(3, 3, 3, 3),
        c = c(3, 2, 2, NA)
    ))

    expect_equal(result_2, data.table(
        x= c(1, 2, 2, 3),
        y= c("A", "B", "B", "C"),
        z = c("I", "J", "J", "K"),
        v = c(1, 2, 2, 3),
        z_y = c("F", "G", "H", NA_character_),
        a = c(3, 3, 3, 3),
        c = c(3, 2, 2, 2)
    ))
})



test_that("LeftJoin$add all works", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:5], y = c(1, 2, 2), z = LETTERS[6:8])

    rel <- Relationship$new(left = x, right=y)$on(x = y)
    join <- LeftJoin$new(rel)
    result <- join$add()

    expect_equal(result, data.table(
        x= c(1, 2, 2, 3),
        y= c("A", "B", "B", "C"),
        z = c("I", "J", "J", "K"),
        v = c(1, 2, 2, 3),
        x_y = c(LETTERS[3:5], NA),
        z_y = c(LETTERS[6:8], NA)
    ))
})


test_that("UpdateJoin$add(where) works", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])

    rel <- Relationship$new(left = x, right=y)$on(x = y)
    join <- UpdateJoin$new(rel)
    join$add(columns = list(a=3, c=ifelse(i.x == 1, 3, 2), z), where=x %in% 1:2)

    expect_equal(x, data.table(
        x= c(1, 2, 3),
        y= c("A", "B", "C"),
        z = c("F", "G","K"),
        v = c(1, 2, 3),
        a = c(3, 3, NA),
        c = c(3, 2, NA)
    ))
})

test_that("UpdateJoin$add works", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])

    rel <- Relationship$new(left = x, right=y)$on(x = y)
    join <- UpdateJoin$new(rel)
    join$add(columns = list(a=3, c=ifelse(i.x == 1, 3, 2), z))

    expect_equal(x, data.table(
        x= c(1, 2, 3),
        y= c("A", "B", "C"),
        z = c(LETTERS[6:7], NA),
        v = c(1, 2, 3),
        a = c(3, 3, 3),
        c = c(3, 2, 2)
    ))
})

test_that("UpdateJoin$add_all(where) works", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])

    rel <- Relationship$new(left = x, right=y)$on(x = y)
    join <- UpdateJoin$new(rel)
    join$add_all(where=x == 1)

    expect_equal(x, data.table(
        x= c(1, 2, 3),
        y= c("A", "B", "C"),
        z = LETTERS[9:11],
        v = c(1, 2, 3),
        x_y = c(LETTERS[3], NA, NA),
        z_y = c(LETTERS[6], NA, NA)
    ))
})

test_that("UpdateJoin$add_all works", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])

    rel <- Relationship$new(left = x, right=y)$on(x = y)
    join <- UpdateJoin$new(rel)
    join$add_all()

    expect_equal(x, data.table(
        x= c(1, 2, 3),
        y= c("A", "B", "C"),
        z = LETTERS[9:11],
        v = c(1, 2, 3),
        x_y = c(LETTERS[3:4], NA),
        z_y = c(LETTERS[6:7], NA)
    ))
})




test_that("UpdateJoin does not work with one-to-many relationship", {
    x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
    y <- data.table(x = LETTERS[3:5], y = c(1, 2, 2), z = LETTERS[6:8])

    rel <- Relationship$new(left = x, right=y)$on(x = y)
    join <- UpdateJoin$new(rel)

    expect_error(join$add(a=3, c=ifelse(i.x == 1, 3, 2), z))
})
