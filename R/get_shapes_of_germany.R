# get_shapes_of_germany --------------------------------------------------------

#' Provide List of SpatialPolygonsDataFrame for Germany
#'
#' @param recreate logical. If \code{TRUE} the required shape files are freshly
#'   downloaded from \url{https://gadm.org/}, transformed to the projection used
#'   in DWD data files and stored locally in an RData file. The default is
#'   \code{FALSE}, i.e. the required data are read form the locally stored RData
#'   file, provided that the file exists.
#' @export
get_shapes_of_germany <- function(recreate = FALSE)
{
  # Set cache directory in subfolder within Windows TEMP folder
  cache_dir <- temp_dir("cache")

  # Path to file in cache in which shapes may have been stored before
  rdata_file <- file.path(cache_dir, "shapes_germany.RData")

  # If the shapes have already been stored before, load and return them
  if (file.exists(rdata_file) && ! recreate) {
    return(kwb.utils::loadObject(rdata_file, "shapes_germany"))
  }

  # List shape files. If required, the shape files are downloaded, unzipped and
  # stored locally. They are downloaded from:
  # https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip
  files <- list_local_shape_files(check_or_download_shapes_germany())[-1L]

  # Read shapes at different levels of detail
  shapes_germany <- lapply(stats::setNames(nm = files), read_shape_file)

  # Load an example Raster of Germany, just to ask for its projection
  example_grid <- get_example_grid_germany()

  # Transform all shapes according to the projection of the example grid
  shapes_germany <- lapply(shapes_germany, sp::spTransform, example_grid@crs)

  # Save the shapes so that next time they can be loaded directly
  save(shapes_germany, file = rdata_file)

  # Return the shapes
  shapes_germany
}

# list_local_shape_files -------------------------------------------------------
list_local_shape_files <- function(path)
{
  dir(path, "shp$", full.names = TRUE)
}

# read_shape_file --------------------------------------------------------------
#' @importFrom rgdal readOGR
read_shape_file <- function(file)
{
  rgdal::readOGR(
    dsn = file,
    stringsAsFactors = FALSE,
    encoding = "UTF-8",
    use_iconv = TRUE
  )
}

