#install.packages("terra")
#remotes::install_github("KWB-R/kwb.dwd@validate-berlin-mask")
#remotes::install_github("hsonne/findblobs")

# Open description file (how to interpret the data?) ---------------------------
if (FALSE)
{
  urls <- kwb.dwd:::list_grids_germany("monthly", ".asc.gz", "evapo_p", "202201")
  kwb.dwd::open_description(urls[1L])
}

# Compare two versions of reading data for Berlin ------------------------------
if (FALSE)
{
  from <- "202105"
  to <- "202107"

  # Version 1: use Andreas Matzinger's Berlin mask matrix
  system.time(result_1 <- kwb.dwd::load_potential_evaporation_berlin(from, to))

  # Version 2: use shape files of German districts to mask Berlin (or any other
  # German city). TODO: Performance needs to be improved (by locally storing
  # transformed shapes)
  system.time(result_2 <- kwb.dwd::load_potential_evaporation_berlin_2(from, to))

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
  # Read shape files for German regions, transformed to projection of DWD grids
  shapes <- kwb.dwd:::get_shapes_of_germany(recreate = TRUE)

  # Create a configuration interactively
  selection <- kwb.dwd:::select_shapes(shapes)

  config_1 <- list(index = 1L, variable = "NAME_1", pattern = "Nordrhein")
  config_2 <- list(index = 2L, variable = "NAME_2", pattern = "Köln")
  config_3 <- list(index = 3L, variable = "NAME_3", pattern = "Köln")
  config_4 <- list(index = 4L, variable = "NAME_4", pattern = "^Köln$")

  raster::plot(s <- kwb.dwd:::filter_shapes(shapes, config_1))
  s@data$NAME_1

  raster::plot(s <- kwb.dwd:::filter_shapes(shapes, config_2))
  s@data$NAME_2

  raster::plot(s <- kwb.dwd:::filter_shapes(shapes, config_3))
  s@data$NAME_3

  raster::plot(s <- kwb.dwd:::filter_shapes(shapes, config_4))
  s@data$NAME_4

  # Two configurations are stored in the package:
  raster::plot(shape_berlin <- get_shape_of_german_region("berlin"))
  raster::plot(shape_cologne <- get_shape_of_german_region("cologne"))

  shp_germany <- shapes[[1L]]
  shp_regions <- shapes[[4L]]

  raster::plot(shp_germany)
  raster::plot(shp_regions)

  grid_example <- kwb.dwd:::get_example_grid_germany()

  extract <- function(pattern) {
    crop_and_mask_region(grid_example, shp_regions, pattern = pattern)
  }

  raster::plot(berlin <- extract("Berlin"))
  raster::plot(cologne <- extract("K\xf6ln"))
  raster::plot(braunschweig <- extract("Braunschweig"))
  raster::plot(munich <- extract("M\xfcnchen$")) # white hole???
}

# Validate Andreas Matzinger's Berlin mask -------------------------------------
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


