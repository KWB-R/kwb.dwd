test_that("url_projection() works", {

  f <- kwb.dwd:::url_projection

  result <- f()

  expect_type(result, "character")
  expect_length(result, 1L)
  expect_true(endsWith(result, ".prj"))
})
