test_that("get_berlin_dwd_mask() works", {

  f <- kwb.dwd:::get_berlin_dwd_mask

  result <- f()

  expect_true(is.matrix(result))
  expect_true(all(is.na(result) | result == 1))
})
