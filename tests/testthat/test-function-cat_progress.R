test_that("cat_progress() works", {

  f <- kwb.dwd:::cat_progress

  expect_error(f())

  expect_output(result <- f(0L, n = 1L), "\\[ \\]")
  expect_null(result)

  expect_output(result <- f(1L, n = 1L))
  expect_null(result)
})
