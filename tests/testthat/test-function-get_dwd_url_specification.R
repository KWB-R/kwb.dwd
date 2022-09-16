test_that("get_dwd_url_specification() works", {

  f <- kwb.dwd:::get_dwd_url_specification

  result <- f()

  expect_true(is.matrix(result))

  expect_identical(
    colnames(result),
    c("file_prefix", "folder_daily", "folder_hourly")
  )

  expect_true("precipitation" %in% rownames(result))
})
