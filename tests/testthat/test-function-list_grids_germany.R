test_that("list_grids_germany() works", {

  f <- kwb.dwd:::list_grids_germany

  expect_error(f())
  expect_error(f("no-such-variable"))

  expect_output(result <- f("sunshine_duration", recursive = FALSE))
  expect_identical(result, character(0))
})
