test_that("temp_dir() works", {

  f <- kwb.dwd:::temp_dir

  result <- f()

  expect_type(result, "character")
  expect_length(result, 1L)
  expect_true(file.exists(result))
})
