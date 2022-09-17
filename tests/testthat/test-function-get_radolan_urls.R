test_that("get_radolan_urls() works", {

  f <- kwb.dwd:::get_radolan_urls

  result <- f()

  all_ftp <- function(x) all(startsWith(x, "ftp://"))

  expect_true(all_ftp(result$daily_historical_urls))
  expect_true(all_ftp(result$hourly_historical_urls))
})
