# read_daily_data_over_shape ---------------------------------------------------

#' Read daily data from DWD, mask region with given shape file
#'
#' Currently, only full months of data can be loaded, \code{from} and \code{to}
#' must be given
#'
#' @param file path to shape file .shp
#' @param variable currently only "evapo_p" is supported
#' @param from first month as "yyyymm" string
#' @param to last month as "yyyymm" string
#' @return data frame
#' @export
read_daily_data_over_shape <- function(file, variable, from, to)
{
  variable <- match.arg(variable, c("evapo_p"))

  # Define scaling factors per variable. Depending on the variable, the values
  # in the data files need to be multiplied with a scaling factor
  scales <- list(
    "evapo_p" = 0.1
  )

  # Select the appropriate scaling factor
  scale <- kwb.utils::selectElements(scales, variable)

  # Read shape file and transform to projection used in DWD's grid files
  shape <- read_shape_with_dwd_projection(file)

  # Get URLs to .tgz files with daily grids on DWD server
  urls <- list_daily_grids_germany(variable, from, to)

  # One URL contains files for one month
  monthly_data_frames <- lapply(urls, get_daily_data_from_one_url, shape, scale)

  do.call(rbind, monthly_data_frames)
}

# read_shape_with_dwd_projection -----------------------------------------------
read_shape_with_dwd_projection <- function(file)
{
  # Read shape file
  shape <- rgdal::readOGR(file)

  # Read example grid from DWD, just to get its projection string
  example_grid <- get_example_grid_germany()

  # Transform the shaps according to the projection of the example grid
  sp::spTransform(shape, example_grid@crs)
}

# get_daily_data_from_one_url --------------------------------------------------
get_daily_data_from_one_url <- function(url, shape, scale)
{
  #kwb.utils::assignPackageObjects("kwb.dwd")
  #urls <- list_daily_grids_germany("evapo_p")
  #url <- urls[1L]

  # Download and extract files from URL
  grid_files <- download_and_extract(url)

  # For each file, provide a projection (.prj) file containing DWD's projection
  lapply(grid_files, provide_projection_file)

  # Read the grids, together with the projection
  grids <- lapply(grid_files, function(file) {
    kwb.utils::catAndRun(paste("Reading", file), raster::raster(file))
  })

  # Provide file metadata (file, year, month, day)
  metadata <- extract_metadata_from_files_daily(files = grid_files)

  # Mask the full grid over Germany with the shape and crop the grid
  grids <- lapply(grids, function(grid) {
    kwb.utils::catAndRun(
      "Masking and cropping",
      raster::crop(raster::mask(grid, shape), shape)
    )
  })

  # Calculate statistics, considering the conversion factor "scale"
  data <- do.call(rbind, lapply(grids, raster_stats, scale = scale))

  # Add metadata
  cbind(metadata, data)
}

# extract_metadata_from_files_daily --------------------------------------------
extract_metadata_from_files_daily <- function(files)
{
  base_names <- basename(files)

  date_parts <- kwb.utils::extractSubstring(
    pattern = "_(\\d{4})(\\d{2})(\\d{2})\\.",
    base_names,
    c(year = 1L, month = 2L, day = 3L)
  )

  date_parts <- as.data.frame(lapply(date_parts, as.integer))

  metadata <- kwb.utils::noFactorDataFrame(
    file = base_names,
    date = as.Date(sprintf(
      "%04d-%02d-%02d",
      date_parts$year, date_parts$month, date_parts$day
    ))
  )

  cbind(metadata, date_parts)
}
