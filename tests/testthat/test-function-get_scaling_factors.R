test_that("get_scaling_factors() works", {

  f <- kwb.dwd:::get_scaling_factors

  result <- f()

  expect_true(is.list(result))
  expect_true(all(sapply(result, is.numeric)))
  expect_true(all(lengths(result) == 1L))
})
