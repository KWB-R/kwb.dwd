# read_daily_data_over_shape ---------------------------------------------------

#' Read daily data from DWD, mask region with given shape file
#'
#' Currently, only full months of data can be loaded, \code{from} and \code{to}
#' must be given
#'
#' @param file path to shape file .shp
#' @param variable currently, the following variables are supported: "evapo_p",
#'   "evapo_r", "frost_depth", "soil_moist", "soil_temperature_5cm"
#' @param from first month as "yyyymm" string
#' @param to last month as "yyyymm" string
#' @param quiet passed to \code{\link{download.file}}
#' @return data frame
#' @export
read_daily_data_over_shape <- function(file, variable, from, to, quiet = FALSE)
{
  # Define scaling factors per variable. Depending on the variable, the values
  # in the data files need to be multiplied with a scaling factor.

  scales <- get_scaling_factors()

  variable <- match.arg(variable, names(scales))

  # Select the appropriate scaling factor
  scale <- kwb.utils::selectElements(scales, variable)

  # Read shape file and transform to projection used in DWD's grid files
  shape <- read_shape_with_dwd_projection(file)

  # Download and extract files from URLs to .tgz files on DWD server
  grid_files <- download_daily_grids_germany(variable, from, to, quiet = quiet)

  # Read data within shape from all grid files
  get_daily_data_from_grid_files(grid_files, shape, scale)
}

# read_shape_with_dwd_projection -----------------------------------------------
read_shape_with_dwd_projection <- function(file, ...)
{
  # Read example grid from DWD, just to get its projection string
  example_grid <- get_example_grid_germany()

  # Read the shape file, transforming the projection to DWD's projection
  read_shape_file(file, target_crs = example_grid@crs, ...)
}

# get_daily_data_from_grid_files -----------------------------------------------
get_daily_data_from_grid_files <- function(grid_files, shape, scale)
{
  # For each file, provide a projection (.prj) file containing DWD's projection
  lapply(grid_files, provide_projection_file)

  # Read the grids, together with the projection
  grids <- lapply(grid_files, function(file) {
    kwb.utils::catAndRun(paste("Reading", file), raster::raster(file))
  })

  # Provide file metadata (file, year, month, day)
  metadata <- extract_metadata_from_files(files = grid_files, is_daily = TRUE)

  # Mask the full grid over Germany with the shape and crop the grid
  grids <- mask_and_crop_grids(grids, shape)

  # Calculate statistics, considering the scaling factor, add metadata
  cbind(metadata, summarise_over_all_grids(grids, scale))
}

# summarise_over_all_grids -----------------------------------------------------
summarise_over_all_grids <- function(grids, scale)
{
  do.call(rbind, lapply(grids, raster_stats, scale = scale))
}

# extract_metadata_from_files --------------------------------------------------
extract_metadata_from_files <- function(files, is_daily)
{
  base_names <- basename(files)

  date_parts <- if (is_daily) {
    kwb.utils::extractSubstring(
      pattern = "_(\\d{4})(\\d{2})(\\d{2})\\.",
      base_names,
      c(year = 1L, month = 2L, day = 3L)
    )
  } else {
    kwb.utils::extractSubstring(
      pattern = "_(\\d{4})(\\d{2})\\.",
      base_names,
      c(year = 1L, month = 2L)
    )
  }

  date_parts <- as.data.frame(lapply(date_parts, as.integer))

  date_strings <- if (is_daily) {
    sprintf(
      "%04d-%02d-%02d",
      date_parts$year, date_parts$month, date_parts$day
    )
  } else {
    sprintf(
      "%04d-%02d-01",
      date_parts$year, date_parts$month
    )
  }

  metadata <- kwb.utils::noFactorDataFrame(
    file = base_names,
    date = as.Date(date_strings)
  )

  cbind(metadata, date_parts)
}

# mask_and_crop_grids ----------------------------------------------------------
mask_and_crop_grids <- function(grids, shape, dbg = TRUE)
{
  lapply(grids, function(grid) {
    kwb.utils::catAndRun(
      paste("Masking and cropping", grid@data@names),
      raster::crop(raster::mask(grid, shape), shape),
      dbg = dbg
    )
  })
}
