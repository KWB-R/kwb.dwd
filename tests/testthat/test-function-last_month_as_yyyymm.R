test_that("last_month_as_yyyymm() works", {

  f <- kwb.dwd:::last_month_as_yyyymm

  result  <- f()

  expect_type(result, "character")
  expect_length(result, 1L)
  expect_true(startsWith(result, substr(Sys.Date(), 1L, 4L)))
})
