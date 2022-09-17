test_that("columns_to_timestamp() works", {

  f <- kwb.dwd:::columns_to_timestamp

  expect_error(f())

  expect_identical(
    f(data.frame(year_or_time = 2022, month = "Apr", day = 1)),
    "2022-04-01 00:00"
  )

})
