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

  # Use the projection of the first grid (identical for all grids)
  shapes <- get_transformed_shapes_of_germany(projection = grids[[1L]]@crs)

  # Create a configuration interactively
  #files <- list_shape_files(check_shapes_germany())[-1L]
  #selection <- select_shapes(shapes, files)
  #writeLines(kwb.utils::objectToText(selection))

  config_berlin <- list(index = 1L, variable = "NAME_1", pattern = "Berlin")

  shape_berlin <- filter_shapes(shapes, config = config_berlin)

  do.call(rbind, lapply(seq_along(grids), function(i) {
    url <- urls[i]
    metadata <- extract_metadata_from_url(url)
    metadata <- kwb.utils::selectElements(metadata, c("file", "year", "month"))
    metadata_df <- kwb.utils::asNoFactorDataFrame(metadata)
    r <- raster::crop(raster::mask(grids[[i]], shape_berlin), shape_berlin)
    #raster::plot(r)
    cbind(metadata_df, raster_stats(r, scale = 0.1))
  }))
}

# get_transformed_shapes_of_germany --------------------------------------------
get_transformed_shapes_of_germany <- function(projection)
{
  # Get path to directory containing shape files. If required, the shape files
  # are downloaded, unzipped and stored locally. They are downloaded from:
  # https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip
  path <- check_shapes_germany()
  files <- list_shape_files(path)[-1L]

  # Read shapes at different levels of detail and ransform the shapes according
  # to the given projection
  lapply(files, function(file) {
    sp::spTransform(read_shape_file(file), CRSobj = projection)
  })
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
