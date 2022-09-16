test_that("get_shapes_of_germany() works", {

  f <- kwb.dwd:::get_shapes_of_germany

  capture.output(result <- f())

  expect_true(is.list(result))
  expect_length(result, 4L)
})
