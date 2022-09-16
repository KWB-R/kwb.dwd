test_that("get_radolan_extension() works", {

  f <- kwb.dwd:::get_radolan_extension

  result <- f()

  expect_type(result, "double")
  expect_length(result, 4L)
})
