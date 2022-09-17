#library(testthat)
test_that("get_shapes_of_germany() works", {

  f <- kwb.dwd:::get_shapes_of_germany

  capture.output(result <- f())

  expect_true(is.list(result))
  expect_length(result, 4L)

  # Running the function again should load from an RData file
  expect_output(f(), "Loading.*from.*RData")
})
