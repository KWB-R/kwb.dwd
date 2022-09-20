test_that("extract_metadata_from_urls() works", {

  f <- kwb.dwd:::extract_metadata_from_urls

  expect_error(f())
  expect_error(f(NULL))

  expect_true(nrow(f("")) == 1L)
  expect_identical(names(f("")), c("year", "month", "file", "origin"))

  expect_identical(
    f("a/bla-202201"),
    kwb.utils::noFactorDataFrame(
      year = 2022L, month = 1L, file = "bla-202201", origin = "a"
    )
  )

  expect_identical(
    f("a/bla-202201", columns = c("year", "month")),
    kwb.utils::noFactorDataFrame(year = 2022L, month = 1L)
  )

})
