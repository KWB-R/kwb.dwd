#library(testthat)

test_that("download() works", {

  f <- kwb.dwd:::download

  expect_error(f())
  expect_error(expect_output(suppressWarnings(f("no-such-url"))))

  url <- default_projection_file(download = FALSE)

  target_dir <- tempdir()

  expect_output(file <- f(url, target_dir = target_dir))
  expect_output(file <- f(url, target_dir = target_dir), "already there")

  expect_true(file.exists(file))

  unlink(file)

  expect_silent(file <- f(url, target_dir = target_dir, quiet = TRUE))

})
