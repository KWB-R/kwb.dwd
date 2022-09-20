test_that("list_monthly_grids_germany_asc_gz() works", {

  f <- kwb.dwd:::list_monthly_grids_germany_asc_gz

  expect_error(f())
  expect_error(f("no-such-variable"))

  expect_output(result <- f("sunshine_duration", recursive = FALSE))
  expect_identical(result, character(0))
})
