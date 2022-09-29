# load_monthly_variable_for_region ---------------------------------------------
load_monthly_variable_for_region <- function(
  variable, region, scale = NULL, from = NULL, to = NULL, version = 1L
)
{
  #kwb.utils::assignPackageObjects("kwb.dwd")

  # Currently, three variables are supported
  variable <- match.arg(variable, c("precipitation", "evapo_p", "evapo_r"))

  # Get URLs to .asc.gz files with monthly grids on DWD server
  urls <- list_monthly_grids_germany_asc_gz(variable, from, to)

  #
  files <- download_monthly_grids_germany(variable, from, to)

  if (version == 1L) {

    region <- match.arg(region, "berlin")

    # Read all files into a list of matrices
    matrices <- lapply(seq_along(urls), function(i) {
      read_asc_gz_file_into_matrix(files[i], scale = scale) %>%
      add_attributes(extract_metadata_from_urls(urls[i]))
    })

    # Get mask matrix for Berlin region
    geo_mask <- get_berlin_dwd_mask()

    # Calculate monthly stats for Berlin
    return(calculate_masked_grid_stats(matrices, geo_mask = geo_mask))
  }

  if (version == 2L) {

    # Read all files into a list of RasterLayer objects
    grids <- lapply(files, read_asc_gz_file)

    # Get shape of region in same projection as grid
    shape <- get_shape_of_german_region(region)

    data_frames <- lapply(grids, function(grid) {
      grid %>%
        raster::mask(shape) %>%
        raster::crop(shape) %>%
        raster_stats(scale = scale)
    })

    metadata <- extract_metadata_from_urls(urls, c("file", "year", "month"))

    return(cbind(metadata, do.call(rbind, data_frames)))
  }

  clean_stop("version must be 1 or 2")
}

# get_berlin_dwd_mask ----------------------------------------------------------

#' Get geographical "stamp" for Berlin area
#'
#' @return ???
#' @export
#' @importFrom utils read.csv
#' @examples
#' get_berlin_dwd_mask()
get_berlin_dwd_mask <- function()
{
  # DWD matrix filled with NA
  berlin_matrix <- matrix(NA, nrow = 866, ncol = 654)

  # file with coordinates of "Berlin cells" within DWD matrix
  file <- system.file("extdata/berlin_coordinates.csv", package = "kwb.dwd")

  # get Berlin coordinates
  berlin_coordinates <- utils::read.csv(file)

  # set Berlin cells to 1
  berlin_matrix[as.matrix(berlin_coordinates)] <- 1

  berlin_matrix
}

# calculate_masked_grid_stats --------------------------------------------------

#' Calculate Stats of Variable for Geographical Subset of a Grid
#'
#' @param matrices matrices
#' @param geo_mask "mask matrix" defining a geographical subset
#'
#' @return data frame with one row per matrix in \code{matrices} and columns
#'   \code{file}, \code{year}, \code{month}, \code{mean}, \code{sd}, \code{min},
#'   \code{max}
#' @export
#' @importFrom kwb.utils getAttribute
#' @importFrom stats sd
calculate_masked_grid_stats <- function(matrices, geo_mask)
{
  # Start with metadata from matrices' attributes: file name, year, month
  result <- get_file_metadata_from_attributes(matrices)

  # Keep only grid cells within "mask"
  masked_values <- lapply(matrices, function(m) m * geo_mask)

  # Function to apply a statistical function to all vectors in berlin_values
  get_stats <- function(fun) sapply(masked_values, fun, na.rm = TRUE)

  result$mean <- get_stats(mean)
  result$sd <- get_stats(stats::sd)
  result$min <- get_stats(min)
  result$max <- get_stats(max)
  result$n_values <- sapply(masked_values, function(x) sum(! is.na(x)))

  result
}

# get_file_metadata_from_attributes --------------------------------------------
get_file_metadata_from_attributes <- function(x)
{
  as.data.frame(lapply(
    stats::setNames(nm = c("file", "year", "month")),
    function(name) sapply(x, kwb.utils::getAttribute, name)
  ))
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

# filter_shapes ----------------------------------------------------------------
filter_shapes <- function(shapes, config)
{
  s <- shapes[[config$index]]
  s[grep(config$pattern, s[[config$variable]]), ]
}

# raster_stats -----------------------------------------------------------------
raster_stats <- function(r, scale = NULL)
{
  stopifnot(inherits(r, "BasicRaster"))

  x <- raster::getValues(r)

  # Just for my understanding: does getValues() return r@data@values?
  if (length(r@data@values) > 0L) {
    stopifnot(identical(r@data@values, x))
  }

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
