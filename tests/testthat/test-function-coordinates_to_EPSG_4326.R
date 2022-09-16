test_that("coordinates_to_EPSG_4326() works", {

  f <- kwb.dwd:::coordinates_to_EPSG_4326

  result <- f(latitude = 1, longitude = 2)

  expect_s3_class(result, "sf")
})
