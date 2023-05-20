#library(testthat)

test_that("get_shapes_of_germany() works", {

  f <- kwb.dwd:::get_shapes_of_germany

  result <- f()

  expect_true(is.list(result))
  expect_length(result, 3L)
  expect_true(all(sapply(result, inherits, "sf")))
})
