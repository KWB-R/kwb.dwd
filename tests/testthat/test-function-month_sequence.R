#library(testthat)

test_that("month_sequence() works", {

  f <- kwb.dwd:::month_sequence

  expect_error(f())

  expect_identical(
    f("202301", "202302"),
    as.Date(c("2023-01-01", "2023-02-01"))
  )

  expect_identical(
    f("202311", "202402"),
    as.Date(c("2023-11-01", "2023-12-01", "2024-01-01", "2024-02-01"))
  )

  expect_identical(
    f("202311", "202402", simple = TRUE),
    c("202311", "202312", "202401", "202402")
  )
})
