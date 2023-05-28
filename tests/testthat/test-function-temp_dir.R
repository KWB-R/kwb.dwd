test_that("temp_dir() works", {

  f <- kwb.dwd:::temp_dir

  result <- f()

  expect_type(result, "character")
  expect_length(result, 1L)
  expect_true(file.exists(result))

  result <- f("test")
  expect_true(file.exists(result))

  expect_error(f("a", "b", template = "x"), "Further arguments .* not allowed")
})
