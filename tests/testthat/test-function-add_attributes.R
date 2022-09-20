test_that("add_attributes() works", {

  f <- kwb.dwd:::add_attributes

  expect_error(f())

  expect_identical(f(1, list()), 1)
  expect_identical(f(1, list(a = 1)), structure(1, a = 1))
})
