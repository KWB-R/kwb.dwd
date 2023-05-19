#library(testthat)

test_that("download_daily_grids_germany() works", {

  f <- kwb.dwd:::download_daily_grids_germany

  expect_error(f())
  expect_error(f("x"), "must be one of 'evapo_p'")

  capture.output(result <- f(variable = "evapo_p", quiet = TRUE))

  expect_type(result, "character")
  expect_true(all(sapply(result, file.exists)))
  expect_true(all(endsWith(result, ".asc")))
})
