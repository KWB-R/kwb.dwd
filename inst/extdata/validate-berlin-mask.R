#install.packages("terra")
#remotes::install_github("KWB-R/kwb.dwd@validate-berlin-mask")
#remotes::install_github("hsonne/findblobs")

# Open description file (how to interpret the data?) ---------------------------
if (FALSE)
{
  base_url <- kwb.dwd:::ftp_path_monthly_grids("evapo_p")
  urls <- kwb.dwd::list_url(base_url, full_names = TRUE)
  (url <- grep("_202201", urls, value = TRUE))

  kwb.dwd::open_description(url)
}

# Compare two versions of reading data for Berlin ------------------------------
if (FALSE)
{
  from <- "199106"
  to <- "202107"

  # Version 1: use Andreas Matzinger's Berlin mask matrix
  result_1 <- kwb.dwd::load_potential_evaporation_berlin(from, to)

  # Version 2: use shape files of German districts to mask Berlin (or any other
  # German city). TODO: Performance needs to be improved (by locally storing
  # transformed shapes)
  result_2 <- kwb.dwd::load_potential_evaporation_berlin_2(from, to)

  # Compare the results of the two versions
  result_1
  result_2
}

# Calculate mean absolute percentage error (MAPE) and plot comparison
if (FALSE)
{
  # Get columns of the 2 versions
  x1 <- result_1$mean
  x2 <- result_2$mean

  # Get length of vector
  n <- dim(result_1)[1]

  # Create matrix containing the 2 columns
  x12_barplot <- matrix(c(x1, x2), byrow=TRUE, nrow=2)
  plot(x1, x2)
  abline(0, 1)

  # Calculate MAPE
  mape <- 100/n * sum((x2-x1)/x2)
  cat(mape, '%')
}

# Read polygons of German cities and apply their masks to Germany --------------
if (FALSE)
{
  # We need a target projection, take it from an example grid of Germany
  grid_example <- kwb.dwd:::get_example_grid_germany()
  shapes <- kwb.dwd:::get_transformed_shapes_of_germany(grid_example@crs)

  # Create a configuration interactively
  files <- kwb.dwd:::list_shape_files(kwb.dwd:::check_shapes_germany())[-1L]
  selection <- kwb.dwd:::select_shapes(shapes, files)
  writeLines(kwb.utils::objectToText(selection))

  config_berlin_1 <- list(index = 1L, variable = "NAME_1", pattern = "Nordrhein")
  config_berlin_2 <- list(index = 2L, variable = "NAME_2", pattern = "Köln")
  config_berlin_3 <- list(index = 3L, variable = "NAME_3", pattern = "Köln")
  config_berlin_4 <- list(index = 4L, variable = "NAME_4", pattern = "^Köln$")

  raster::plot(s <- kwb.dwd:::filter_shapes(shapes, config = config_berlin_1))
  s@data$NAME_1

  raster::plot(s <- kwb.dwd:::filter_shapes(shapes, config = config_berlin_2))
  s@data$NAME_2

  raster::plot(s <- kwb.dwd:::filter_shapes(shapes, config = config_berlin_3))
  s@data$NAME_3

  raster::plot(s <- kwb.dwd:::filter_shapes(shapes, config = config_berlin_4))
  s@data$NAME_4

  shp_germany <- shapes[[1L]]
  shp_regions <- shapes[[4L]]

  raster::plot(shp_germany)
  raster::plot(shp_regions)

  extract <- function(pattern) {
    crop_and_mask_region(grid_example, shp_regions, pattern = pattern)
  }

  raster::plot(berlin <- extract("Berlin"))
  raster::plot(cologne <- extract("K\xf6ln"))
  raster::plot(braunschweig <- extract("Braunschweig"))
  raster::plot(munich <- extract("M\xfcnchen$")) # white hole???
}

# Validat Andreas Matzinger's Berlin mask --------------------------------------
if (FALSE)
{
  # What is the difference between the Berlin "mask" of Andreas and the mask
  # resulting from raster::mask()?
  grid_example <- kwb.dwd::get_example_grid_germany()

  # Generate matrix similar to kwb.dwd::get_berlin_dwd_mask()
  mask_from_shape <- get_region_mask_matrix(grid_example, shp_regions, "Berlin")
  mask_andreas <- kwb.dwd::get_berlin_dwd_mask()

  dim(mask_from_shape)
  dim(mask_andreas)

  sum(! is.na(mask_from_shape))
  sum(! is.na(mask_andreas))

  m <- overlay_masks(m1 = mask_from_shape, m2 = mask_andreas)
  m2 <- kwb.utils::clipMatrix(m)

  r <- raster::raster(m)
  r2 <- raster::raster(m2)

  plot(r)
  plot(r2)

  findblobs::plot_integer_matrix(m2)
  View(m2)
}

# Read shape file using terra package ------------------------------------------
if (FALSE)
{
  # Read shape file
  v <- terra::vect(files[3L])

  terra::plot(v)
}

# crop_and_mask_region ---------------------------------------------------------
crop_and_mask_region <- function(r, shp, pattern)
{
  s <- filter_region(shp, pattern)
  raster::crop(raster::mask(r, s), s)
}

# filter_region ----------------------------------------------------------------
filter_region <- function(shp, pattern)
{
  shp[grep(pattern, shp$NAME_2), ]
}

# get_region_mask_matrix -------------------------------------------------------
get_region_mask_matrix <- function(grid_germany, shp_regions, pattern)
{
  x <- raster::mask(grid_germany, filter_region(shp_regions, pattern))
  matrix(x@data@values, nrow = x@nrows, byrow = TRUE)
}

# overlay_masks ----------------------------------------------------------------
overlay_masks <- function(m1, m2)
{
  m <- m1
  m[] <- NA_integer_

  m[! is.na(m1) & is.na(m2)] <- 1L
  m[is.na(m1) & ! is.na(m2)] <- 2L
  m[! is.na(m1) & ! is.na(m2)] <- 3L

  m
}


