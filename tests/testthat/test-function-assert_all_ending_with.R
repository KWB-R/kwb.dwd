test_that("assert_all_ending_with() works", {

  f <- kwb.dwd:::assert_all_ending_with

  expect_error(f())
  expect_error(f("abc"))
  expect_silent(f("abc", "c"))
  expect_silent(f("abc.gz", ".gz"))
})
