test_that("download_and_extract() works", {

  f <- kwb.dwd:::download_and_extract

  capture.output(url <- kwb.dwd:::list_grids_germany("daily", ".tgz", "evapo_p")[1L])
  capture.output(result <- f(url, quiet = TRUE))

  expect_type(result, "character")
  expect_true(all(sapply(result, file.exists)))
})
