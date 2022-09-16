test_that("write_raster_to_file() works", {

  f <- kwb.dwd:::write_raster_to_file

  expect_error(f())

})
