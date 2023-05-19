#library(testthat)

test_that("list_grids_germany() works", {

  f <- kwb.dwd:::list_grids_germany

  expect_error(f())

  expect_error(
    f("hourly"),
    "must be one of 'monthly', 'daily'"
  )

  expect_error(
    f("daily", ".tgz", "sunshine_duration"),
    "must be one of 'evapo_p'"
  )

  expect_output(
    result <- f("monthly", ".tgz", "sunshine_duration", recursive = FALSE)
  )

  expect_identical(result, character(0))
})
