test_that("ftp_path_monthly_grids() works", {

  f <- kwb.dwd:::ftp_path_monthly_grids

  result <- f()

  expect_type(result, "character")
  expect_length(result, 1L)
  expect_true(startsWith(result, "ftp://"))
})
