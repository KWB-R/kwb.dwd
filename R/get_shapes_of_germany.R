# get_shapes_of_germany --------------------------------------------------------

#' Provide List of SpatialPolygonsDataFrame for Germany
#'
#' @param recreate logical. If \code{TRUE} the required shape files are freshly
#'   downloaded from <https://gadm.org/>, transformed to the projection used
#'   in DWD data files and stored locally in an RData file. The default is
#'   \code{FALSE}, i.e. the required data are read form the locally stored RData
#'   file, provided that the file exists.
#' @param use_sf passed to \code{kwb.dwd:::transform_coords}
#' @export
get_shapes_of_germany <- function(recreate = FALSE, use_sf = FALSE)
{
  if (!recreate) {

    # Create the list from the datasets that are stored in this package
    return(list(
      gadm40_DEU_0 = kwb.dwd::shapes_germany_0,
      gadm40_DEU_1 = kwb.dwd::shapes_germany_1,
      gadm40_DEU_2 = kwb.dwd::shapes_germany_2
    ))
  }

  #kwb.utils::assignPackageObjects("kwb.dwd")

  # Set cache directory in sub folder within Windows TEMP folder
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
  shape_dir <- check_or_download_shapes_germany()
  files <- list_local_shape_files(shape_dir)

  # Read shapes at different levels of detail
  shapes_germany <- lapply(
    X = stats::setNames(files, kwb.utils::removeExtension(basename(files))),
    #FUN = read_shape_file_2
    FUN = read_shape_file,
    use_sf = TRUE
  )

  # Load an example Raster of Germany, just to ask for its projection
  example_grid <- get_example_grid_germany()

  #crs_lines <- readLines("https://opendata.dwd.de/climate_environment/CDC/help/gk3.prj")
  #print(example_grid@crs)
  #cat(crs_lines)

  # Transform all shapes according to the projection of the example grid
  shapes_germany <- lapply(
    shapes_germany,
    FUN = transform_coords,
    target_crs = example_grid@crs,
    use_sf = use_sf
  )

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
