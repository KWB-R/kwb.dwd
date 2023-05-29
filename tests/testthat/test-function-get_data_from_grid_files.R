#library(testthat)

test_that("get_data_from_grid_files() works", {

  f <- kwb.dwd:::get_data_from_grid_files

  expect_error(f())
  expect_error(f("no-such-resolution"))
})
