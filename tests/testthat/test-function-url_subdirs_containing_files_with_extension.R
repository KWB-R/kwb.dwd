test_that("url_subdirs_containing_files_with_extension() works", {

  f <- kwb.dwd:::url_subdirs_containing_files_with_extension

  expect_error(f())
})
