if (FALSE)
{
  `%>%` <- magrittr::`%>%`

  shape_file <- "~/../Downloads/A/amarex/GIS-Verschneidung/EZG_Berlin_BWB_shape.shp"

  system.time(evapo_p_daily <- kwb.dwd::read_daily_data_over_shape(
    file = shape_file,
    variable = "evapo_p",
    from = "202001",
    to = "202002"
  ))
  #  User      System verstrichen
  # 32.27        4.37       37.36

  system.time(evapo_p_monthly <- kwb.dwd::read_monthly_data_over_shape(
    file = shape_file,
    variable = "evapo_p",
    from = "202001",
    to = "202002",
    drop_z = TRUE, # passed to kwb.dwd:::read_shape_file()
    use_sf = TRUE  # passed to kwb.dwd:::read_shape_file()
  ))
  # User      System verstrichen
  # 1.71        0.36        4.08

  # We should find the monthly data when summarising the daily data...
  evapo_p_daily %>%
    dplyr::group_by(
      .data$year,
      .data$month
    ) %>%
    dplyr::summarise(
      sum_of_mean = sum(.data$mean)
    )

  # Compare with monthly: not exactly the same...
  evapo_p_monthly

  # Read shape file as Spatial object and transform to projection used by DWD
  shape_spatial <- kwb.dwd:::read_shape_with_dwd_projection(
    shape_file,
    use_sf = TRUE,
    drop_z = TRUE,
    as_spatial = TRUE
  )

  # Filter for features
  shape_spatial <- shape_spatial[grepl(" (X|VII)$", shape_spatial$Pumpwerk), ]

  sp::plot(shape_spatial)

  grid <- kwb.dwd::get_example_grid_germany()

  sp::plot(grid)

  shaped_grid <- crop_and_mask(grid, shape_spatial)

  sp::plot(grid)
  sp::plot(shaped_grid)
  sp::plot(shape_spatial, add = TRUE)

  as.matrix(shaped_grid)
}

crop_and_mask <- function(grid, shape_spatial)
{
  `%>%` <- magrittr::`%>%`

  result <- grid %>%
    raster::crop(shape_spatial) %>%
    raster::mask(shape_spatial)

  result2 <- kwb.dwd:::mask_and_crop_grids(list(grid), shape_spatial)

  stopifnot(identical(result, result2))

  result
}

as.matrix.RasterLayer <- function(x)
{
  matrix(x@data@values, nrow = x@nrows, byrow = TRUE)
}
