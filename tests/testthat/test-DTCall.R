test_that("setting arguments in DTCall works",
          {
              e <- DTCall$new(tbl)
              e$set(i = quote(a==3), j = quote(.SD))

              expect_equal(e$call(), quote(`[`(x= tbl, i = a == 3, j = .SD)))


          })

test_that("renaming table works through DTCall$set",
          {
              e <- DTCall$new(tbl)
              e$set(x=quote(y), i = quote(a==2))
              expect_equal(e$call(), quote(`[`(x= y, i= a==2)))
          })

test_that("removing and overriding arguments in DTCall works",
          {
              e <- DTCall$new(tbl)
              e$set(i = NULL, j = NULL)
              expect_equal(e$call(), quote(tbl))

              e$set(i = quote(a==3), j = quote(.SD))
              e$set(i = NULL, j = quote(`:=` (y = x)))
              expect_equal(e$call(), quote(`[`(x= tbl, j = `:=`(y = x))))
          })


test_that("getting arguments in DTCall works",
          {
              e <- DTCall$new(tbl)
              e$set(i = quote(a==3), j = quote(.SD))

              expect_error(e$get(c("i", "j")))

              expect_null(e$get(c("by")))

              expect_equal(e$get("i"), quote(a == 3))
          })


test_that("call returns only x (the name of the table) if neither i nor j are specified - to prevent data.table warning",
          {
              e <- DTCall$new(tbl)

              e$set(by=quote(a))

              expect_equal(e$call(), quote(tbl))
          })

test_that("call subset works",
          {
              e <- DTCall$new(tbl)
              e$set(i = quote(a==3), j = quote(.SD), by=quote(a))

              expect_equal(e$call(), quote(`[`(x=tbl, i=a==3, j=.SD, by=a)))
              expect_equal(e$call(subset="i"), quote(`[`(x=tbl, i=a==3)))
          })


test_that("by and keyby are mutually exclusive with warning upon override",
          {
              e <- DTCall$new(tbl)
              e$set(i=quote(a), by=quote(a))

              expect_warning(e$set(keyby=quote(a)))
              expect_equal(e$call(), quote(`[`(x=tbl, i=a, keyby=a)))
              expect_warning(e$set(by=quote(a)))
              expect_equal(e$call(), quote(`[`(x=tbl, i=a, by=a)))
          })
