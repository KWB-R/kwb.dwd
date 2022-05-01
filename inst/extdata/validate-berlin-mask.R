#install.packages("terra")
#remotes::install_github("KWB-R/kwb.dwd@dev")
#remotes::install_github("hsonne/findblobs")

# Read a RasterLayer from DWD --------------------------------------------------
if (FALSE)
{
  evap_p <- kwb.dwd::load_potential_evaporation_berlin("202201", "202201")

  url_evap_p <- kwb.dwd:::ftp_path_monthly_grids("evapo_p")
  url <- file.path(url_evap_p, evap_p$file[1L])

  #kwb.dwd::open_description(url)

  # Read file into RasterLayer object
  grid_germany <- kwb.dwd::read_asc_gz_file(url = url)

  raster::plot(grid_germany)
}

# Read polygons of German cities -----------------------------------------------
if (FALSE)
{
  path <- "~/../Downloads/G/gadm40_DEU_shp"
  files <- list_shape_files(path)

  transform <- function(x) sp::spTransform(x, grid_germany@crs)

  shp_germany <- transform(read_shape_file(files[1L]))
  shp_regions <- transform(read_shape_file(files[4L]))

  extract <- function(pattern) {
    crop_and_mask_region(grid_germany, shp_regions, pattern = pattern)
  }

  berlin <- extract("Berlin")
  cologne <- extract("K\xf6ln")
  braunschweig <- extract("Braunschweig")
  munich <- extract("M\xfcnchen$")

  raster::plot(berlin)
  raster::plot(cologne)
  raster::plot(braunschweig)
  raster::plot(munich)

  # What is the difference between the Berlin "mask" of Andreas and the
  # mask resulting from raster::mask()?

  # Generate matrix similar to kwb.dwd::get_berlin_dwd_mask()
  mask_from_shape <- get_region_mask_matrix(grid_germany, shp_regions, "Berlin")
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

  str(berlin)
  evap_p

  x <- berlin@data@values/10
  mean(x, na.rm = TRUE)
  min(x, na.rm = TRUE)
  max(x, na.rm = TRUE)
  sd(x, na.rm = TRUE)

  dim(m)
  dim(mask_berlin)
  i <- ! is.na(m)
  mask_berlin[i] <- mask_berlin[i] + 1L
  j <- ! is.na(m) & is.na(mask_berlin)
  mask_berlin[j] <- -1
  table(mask_berlin)

  findblobs::plot_integer_matrix(mask_berlin)
  View(mask_berlin)

  mask_berlin <- kwb.dwd:::get_berlin_dwd_mask()
  str(mask_berlin)

  head(which(!is.na(m), arr.ind = TRUE))
  head(which(!is.na(mask_berlin), arr.ind = TRUE))

  sum(!is.na(mask_berlin))
  sum(!is.na(berlin@data@values))
}

# Read shape file using terra package ------------------------------------------
if (FALSE)
{
  # Read shape file
  v <- terra::vect(files[3L])

  terra::plot(v)
}

# list_shape_files -------------------------------------------------------------
list_shape_files <- function(path)
{
  dir(path, "shp$", full.names = TRUE)
}

# read_shape_file --------------------------------------------------------------
read_shape_file <- function(file)
{
  rgdal::readOGR(
    dsn = file,
    stringsAsFactors = FALSE,
    encoding = "UTF-8",
    use_iconv = TRUE
  )
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

