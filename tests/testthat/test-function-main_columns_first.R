test_that("main_columns_first() works", {

  f <- kwb.dwd:::main_columns_first

  expect_error(f())

  x <- data.frame(a = 1, b = 2)

  expect_identical(f(x), x)

  x <- data.frame(isdir = TRUE, file = "a")

  expect_identical(f(x), x[, 2:1])
})
