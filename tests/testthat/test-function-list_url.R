test_that("list_url() works", {

  f <- kwb.dwd:::list_url

  capture.output(result <- f())

  expect_type(result, "character")
  expect_true("grids_germany/" %in% result)
})
