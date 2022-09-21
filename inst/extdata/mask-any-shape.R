if (FALSE)
{
  `%>%` <- magrittr::`%>%`

  # Read monthly data for the whole of Germany
  rain_2012 <- kwb.dwd:::read_monthly_data_over_shape(
    variable = "precipitation",
    from = "202101",
    to = "202112"
  )

  head(rain_2012)
  warnings()

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

  # Read the shape file on your own and pass the spatial object in "shape"
  shape <- kwb.dwd:::read_shape_file(shape_file, drop_z = TRUE, use_sf = TRUE)

  system.time(evapo_p_monthly2 <- kwb.dwd::read_monthly_data_over_shape(
    shape = shape,
    variable = "evapo_p",
    from = "202001",
    to = "202002"
  ))

  identical(evapo_p_monthly, evapo_p_monthly2)

  # Now, the same with a subset of the spatial object
  system.time(evapo_p_monthly_bln_x <- kwb.dwd::read_monthly_data_over_shape(
    shape = shape[shape$Pumpwerk == "APw Bln X", ],
    variable = "evapo_p",
    from = "202001",
    to = "202002"
  ))

  # Rainfall in the different Berliner Bezirke
  zip_file_bezirke_berlin <- kwb.dwd:::download_if_not_there(
    "https://tsb-opendata.s3.eu-central-1.amazonaws.com/bezirksgrenzen/bezirksgrenzen.shp.zip"
  )

  shape_dir_bezirke <- kwb.dwd:::temp_dir("bezirke")

  archive::archive_extract(
    zip_file_bezirke_berlin,
    dir = shape_dir_bezirke
  )

  shapes_bezirke <- kwb.dwd:::read_shape_file(
    file.path(shape_dir_bezirke, "bezirksgrenzen.shp"),
    use_sf = TRUE
  )

  sp::plot(shapes_bezirke)

  shapes_by_bezirk <- split(shapes_bezirke, shapes_bezirke$Gemeinde_n)

  rain_by_bezirk <- lapply(shapes_by_bezirk, function(shape) {
    kwb.dwd::read_monthly_data_over_shape(
      variable = "precipitation",
      from = "202001",
      to = "202012",
      shape = shape
    )
  })

  rain_by_bezirk_df <- kwb.utils::rbindAll(
    rain_by_bezirk,
    nameColumn = "bezirk"
  )

  View(rain_by_bezirk_df)

  library(ggplot2)
  library(magrittr)

  rain_by_bezirk_df %>%
    ggplot(aes(
      x = as.factor(.data$month),
      y = .data$mean
    )) +
    geom_bar(stat = 'identity', position = 'dodge') +
    facet_wrap(~ .data$bezirk) +
    labs(x = "month", y = "precipitation in mm")

  # Problem: the download is repeated for each catchment!

  evapo_p_monthly
  evapo_p_monthly_bln_x

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
