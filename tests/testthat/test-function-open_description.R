test_that("open_description() works", {

  f <- kwb.dwd:::open_description

  expect_error(f())

  expect_output(result <- f(kwb.dwd:::ftp_path_daily_grids("evapo_p")))
  expect_null(result)
})
