test_that("get_dwd_config_combinations() works", {

  f <- kwb.dwd:::get_dwd_config_combinations

  result <- f()

  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c("frequency", "category", "currentness"))
})
