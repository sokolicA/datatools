test_that("merge_calls works", {
  expect_error(merge_calls(quote(list("a")), quote(c("a"))))
  expect_equal(merge_calls(NULL, NULL), NULL)
  expect_equal(merge_calls(quote(list(a)), quote(list(b))), quote(list(a, b)))
  expect_equal(merge_calls(quote(list(a)), quote(NULL)), quote(list(a)))
  expect_equal(merge_calls(quote(NULL), quote(list(a))), quote(list(a)))
})


