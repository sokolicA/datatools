test_that("setting arguments in DTCall works",
          {
              e <- DTCall$new(tbl)
              e$set(i = quote(a==3), j = quote(.SD))

              expect_equal(e$call(), quote(`[`(x= tbl, i = a == 3, j = .SD)))


          })

test_that("renaming table works through DTCall$set",
          {
              e <- DTCall$new(tbl)
              e$set(x=quote(y))
              expect_equal(e$call(), quote(`[`(x= y)))
          })

test_that("removing and overriding arguments in DTCall works",
          {
              e <- DTCall$new(tbl)
              e$set(i = NULL, j = NULL)
              expect_equal(e$call(), quote(`[`(x= tbl)))

              e$set(i = quote(a==3), j = quote(.SD))
              e$set(i = NULL, j = quote(`:=` (y = x)))
              expect_equal(e$call(), quote(`[`(x= tbl, j = `:=`(y = x))))
          })
