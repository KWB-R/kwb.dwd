test_that("cat0() works", {

  f <- kwb.dwd:::cat0

  expect_output(f("abc", "def"), "abcdef")

})
