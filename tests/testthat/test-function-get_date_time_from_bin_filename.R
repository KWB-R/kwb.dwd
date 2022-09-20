test_that("get_date_time_from_bin_filename() works", {

  f <- kwb.dwd:::get_date_time_from_bin_filename

  expect_error(f())

  expect_warning(result <- f("x"))

  expect_s3_class(result, "POSIXct")

  expect_identical(
    f("raa01-sf_10000-2212011200-dwd---bin"),
    as.POSIXct("2022-12-01 12:00", tz = "UTC")
  )
})
