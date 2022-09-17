test_that("default_projection_file() works", {

  f <- kwb.dwd:::default_projection_file

  result <- f()

  expect_identical(kwb.utils::fileExtension(result), "prj")

})
