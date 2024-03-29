test_that("download_monthly_grids_germany() works", {

  f <- kwb.dwd:::download_monthly_grids_germany

  expect_error(f())
  expect_error(f("unknown-variable"), "should be one of")

  expect_output(result <- f("evapo_p"))

  expect_length(result, 1L)
  expect_true(file.exists(result))
})
