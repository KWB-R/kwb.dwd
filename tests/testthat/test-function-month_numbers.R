test_that("month_numbers() works", {

  f <- kwb.dwd:::month_numbers

  result <- f()

  expect_true(is.list(result))
  expect_identical(as.integer(result), 1:12)
})
