test_that("read_binary_radolan_file_raw_v1() works", {

  f <- kwb.dwd:::read_binary_radolan_file_raw_v1

  expect_error(f())
  expect_error(expect_warning(f("no-such-file")))

  # Create a non-valid test file
  file <- tempfile("test-")
  writeLines("this is a test", file)

  expect_error(f(file))
})
