#library(testthat)

test_that("list_files_in_zip_files() works", {

  f <- kwb.dwd:::list_files_in_zip_files

  expect_error(f())

  expect_error(f(c("no-such-file-1", "no-such-file-2"), dbg = FALSE))
})
