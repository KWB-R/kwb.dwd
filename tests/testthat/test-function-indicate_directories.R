test_that("indicate_directories() works", {

  f <- kwb.dwd:::indicate_directories

  expect_error(f())

  expect_identical(f(character(0)), character(0))

  expect_identical(f("a", TRUE), "a/")
  expect_identical(f("a", FALSE), "a")

})
