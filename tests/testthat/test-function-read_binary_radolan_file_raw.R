test_that("read_binary_radolan_file_raw() works", {

  f <- kwb.dwd:::read_binary_radolan_file_raw

  expect_error(f())
  expect_error(f(version = 1L))
  expect_error(f(version = 2L))
})
