test_that("get_dwd_urls_metadata() works", {

  f <- kwb.dwd:::get_dwd_urls_metadata

  result <- f()

  expect_type(result, "character")
  expect_true(!is.null(names(result)))
  expect_true(!all(is.na(names(result))))
})
