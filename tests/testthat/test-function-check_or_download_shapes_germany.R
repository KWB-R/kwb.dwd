#library(testthat)

test_that("check_or_download_shapes_germany() works", {

  f <- kwb.dwd:::check_or_download_shapes_germany

  expect_error(
    shape_file <- f(quiet = TRUE, timeout = 1),
    "Could not download .* within 1 seconds"
  )
})
