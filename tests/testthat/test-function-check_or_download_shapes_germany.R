#library(testthat)

test_that("check_or_download_shapes_germany() works", {

  f <- kwb.dwd:::check_or_download_shapes_germany

  result <- try(f(quiet = FALSE, timeout = 1))

  if (kwb.utils::isTryError(result)) {

    expect_match(
      as.character(result),
      "Could not download .* within 1 seconds"
    )


  } else {

    expect_true(file.exists(result))
    #unlink(result, recursive = TRUE)
    #unlink("~/../Downloads/shapes_germany/gadm40_DEU_shp.zip")
  }

})
