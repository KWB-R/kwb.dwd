test_that("get_radolan_projection_string() works", {

  f <- kwb.dwd:::get_radolan_projection_string

  result <- f()

  expect_type(result, "character")
  expect_true(startsWith(result, "+proj="))
})
