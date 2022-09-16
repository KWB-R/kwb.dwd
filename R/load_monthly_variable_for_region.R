# load_monthly_variable_for_region ---------------------------------------------
load_monthly_variable_for_region <- function(
  variable, region, scale = NULL, from = NULL, to = NULL, version = 1L
)
{
  # Currently, three variables are supported
  match.arg(variable, c("precipitation", "evapo_p", "evapo_r"))

  # Get URLs to .asc.gz files with monthly grids on DWD server
  base_url <- ftp_path_monthly_grids(variable)
  urls <- list_zipped_esri_ascii_grids(base_url, from, to)

  if (version == 1L) {

    match.arg(region, "berlin")

    # Read all files into a list of matrices
    matrices <- lapply(urls, read_asc_gz_file_into_matrix, scale = scale)

    # Get mask matrix for Berlin region
    geo_mask <- get_berlin_dwd_mask()

    # Calculate monthly stats for Berlin
    return (calculate_masked_grid_stats(matrices, geo_mask = geo_mask))
  }

  if (version == 2L) {

    # Read all files into a list of RasterLayer objects
    grids <- lapply(urls, read_asc_gz_file, file = NULL)

    # Get shape of region in same projection as grid
    shape <- get_shape_of_german_region(region)

    return (cbind(
      extract_metadata_from_urls(urls, c("file", "year", "month")),
      do.call(rbind, lapply(grids, function(grid) {
        raster_stats(
          raster::crop(raster::mask(grid, shape), shape),
          scale = scale
        )
      }))
    ))
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
  #DWD matrix filled with NA
  berlin_matrix <- matrix(NA, nrow = 866, ncol = 654)

  #get Berlin coordinates
  berlin_coordinates <- utils::read.csv(system.file(
    "extdata/berlin_coordinates.csv", package = "kwb.dwd"
  ))

  #set Berlin cells to 1

  for (i in seq_along(berlin_coordinates$row)) {
    berlin_matrix[berlin_coordinates$row[i], berlin_coordinates$col[i]] <- 1
  }

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
