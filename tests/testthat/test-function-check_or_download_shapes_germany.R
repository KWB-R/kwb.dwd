test_that("check_or_download_shapes_germany() works", {

  f <- kwb.dwd:::check_or_download_shapes_germany

  capture.output(result <- f(quiet = TRUE))

  expect_true(file.exists(result))
})
