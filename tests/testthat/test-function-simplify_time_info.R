test_that("simplify_time_info() works", {

  f <- kwb.dwd:::simplify_time_info

  expect_error(f())

  expect_identical(
    f(data.frame(year_or_time = 1975L, month = "Jan", day = 14L)),
    kwb.utils::noFactorDataFrame(modification_time = "1975-01-14 00:00")
  )
})
