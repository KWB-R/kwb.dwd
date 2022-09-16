test_that("ftp_path_cdc() works", {

  f <- kwb.dwd:::ftp_path_cdc

  result <- f()

  expect_type(result, "character")
  expect_length(result, 1L)
  expect_true(startsWith(result, "ftp://"))
})
