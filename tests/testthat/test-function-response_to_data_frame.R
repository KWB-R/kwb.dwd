test_that("response_to_data_frame() works", {

  f <- kwb.dwd:::response_to_data_frame

  expect_error(f())


  empty_info_small <- kwb.dwd:::empty_file_info(full_info = FALSE)
  empty_info_full <- kwb.dwd:::empty_file_info(full_info = TRUE)

  expect_identical(f(NULL), empty_info_small)
  expect_identical(f(""), empty_info_small)
  expect_identical(f("   "), empty_info_small)
})
