test_that("dwd_url_climate_dir() works", {

  f <- kwb.dwd:::dwd_url_climate_dir

  result <- f()

  expect_length(result, 1L)
  expect_type(result, "character")
  expect_true(startsWith(result, "ftp://"))

  expect_ftp <- function(x) expect_true(startsWith(x, "ftp://"))

  expect_ftp(f("daily"))

  expect_error(f("cats"), "frequency .* must be one of")
  expect_error(f("daily", category = "dogs"), "category .* must be one of")

  expect_ftp(f("daily", "sun"))
})
