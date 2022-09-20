test_that("read_relevant_years_radolan() works", {

  f <- kwb.dwd:::read_relevant_years_radolan

  expect_error(f())
  expect_error(f(years = 1999L))
  expect_error(f("/no/such/path", years = 2000L))

  expect_message(result <- f(tempdir(), years = 2000L))
  expect_null(result)

  writeLines("this is a test", file.path(tempdir(), "0-test.gri"))

  expect_error(result <- f(tempdir(), 2000L), "Cannot create a RasterLayer")
  expect_null(result)
})
