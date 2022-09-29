#library(testthat)

test_that("raster_stats() works", {

  f <- kwb.dwd:::raster_stats

  expect_error(f())
  expect_error(f(1))

  r <- raster::raster(matrix(1:12, nrow = 3))

  result <- f(r)

  expect_identical(
    colnames(result),
    c("mean", "sd", "min", "max", "n_values")
  )
})
