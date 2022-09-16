test_that("read_shape_with_dwd_projection() works", {

  f <- kwb.dwd:::read_shape_with_dwd_projection

  expect_error(capture.output(f()))

})
