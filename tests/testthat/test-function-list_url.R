test_that("list_url() works", {

  f <- kwb.dwd:::list_url

  capture.output(result <- f())
  capture.output(result2 <- f(full_info = TRUE))

  expect_type(result, "character")
  expect_true("grids_germany/" %in% result)
})
