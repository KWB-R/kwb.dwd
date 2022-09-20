test_that("assert_ending_gz() works", {

  f <- kwb.dwd:::assert_ending_gz

  expect_error(f())
  expect_error(f("abc"))
  expect_silent(f("abc.gz"))
})
