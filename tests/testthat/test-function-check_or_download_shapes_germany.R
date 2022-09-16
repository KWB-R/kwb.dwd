test_that("check_or_download_shapes_germany() works", {

  f <- kwb.dwd:::check_or_download_shapes_germany

  result <- f()

  expect_true(file.exists(result))
})
