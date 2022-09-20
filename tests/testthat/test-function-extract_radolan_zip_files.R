test_that("extract_radolan_zip_files() works", {

  f <- kwb.dwd:::extract_radolan_zip_files

  expect_error(f())

  expect_output(result <- f(tempdir()))
})
