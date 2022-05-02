# load_potential_evaporation_berlin_2 ------------------------------------------

#' New Version of Reading Potential Evaporation for Berlin
#'
#' This version uses publicly available shape files to determine the area that
#' is covered by Berlin. This function can easily be adopted to other cities!
#'
#' @param from optional. First month to be considered, as yyyymm string
#' @param to optional. Last month to be considered, as yyyymm string
#' @return data frame with columns \code{file} (name of file downloaded from
#'   DWD), \code{year} (year number as integer), \code{month number as integer},
#'   \code{mean} (mean value), \code{sd} (standard deviation), \code{min}
#'   (minimum value), \code{max} (maximum value), \code{n_values} (number of
#'   considered values) of potential evaporation calculated for Berlin, Germany
#' @export
load_potential_evaporation_berlin_2 <- function(from, to)
{
  #from="202106";to="202108"

  # Get URLs to data files
  base_url <- ftp_path_monthly_grids("evapo_p")
  urls <- list_zipped_esri_ascii_grids(base_url, from, to)

  # Read all files into a list of RasterLayer objects
  grids <- lapply(urls, read_asc_gz_file, file = NULL)

  # Get shape of Berlin in same projection as grid
  shape <- get_shape_of_german_region("berlin")

  cbind(
    extract_metadata_from_urls(urls, c("file", "year", "month")),
    do.call(rbind, lapply(grids, function(grid) {
      raster_stats(raster::crop(raster::mask(grid, shape), shape), scale = 0.1)
    }))
  )
}

# get_shape_of_german_region ---------------------------------------------------
get_shape_of_german_region <- function(name)
{
  configure <- function(index, variable, pattern) list(
    index = index,
    variable = variable,
    pattern = pattern
  )

  configs <- list(
    berlin = configure(1L, "NAME_1", "Berlin"),
    cologne = configure(2L, "NAME_2", "K\xF6ln")
  )

  config <- kwb.utils::selectElements(configs, name)

  filter_shapes(get_shapes_of_germany(), config = config)
}

# get_shapes_of_germany --------------------------------------------------------
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

  # Get path to directory containing shape files. If required, the shape files
  # are downloaded, unzipped and stored locally. They are downloaded from:
  # https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip
  files <- list_shape_files(check_shapes_germany())[-1L]

  # Read shapes at different levels of detail and transform the shapes according
  # to the same projection that is assigned to the grid
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

# list_shape_files -------------------------------------------------------------
list_shape_files <- function(path)
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

# filter_shapes ----------------------------------------------------------------
filter_shapes <- function(shapes, config)
{
  s <- shapes[[config$index]]
  s[grep(config$pattern, s[[config$variable]]), ]
}


# raster_stats -----------------------------------------------------------------
raster_stats <- function(r, scale = NULL)
{
  x <- r@data@values
  x <- x[! is.na(x)]

  if (! is.null(scale)) {
    x <- x * scale
  }

  rng <- range(x)

  cbind(
    mean = mean(x),
    sd = sd(x),
    min = rng[1L],
    max = rng[2L],
    n_values = length(x)
  )
}
