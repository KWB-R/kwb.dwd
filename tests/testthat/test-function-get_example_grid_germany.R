test_that("get_example_grid_germany() works", {

  f <- kwb.dwd:::get_example_grid_germany

  capture.output(suppressWarnings(result <- f()))

  expect_true(inherits(result, "RasterLayer"))
})
