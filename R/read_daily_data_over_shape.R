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
#' @return data frame
#' @export
read_daily_data_over_shape <- function(file, variable, from, to)
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
  grid_files <- download_daily_grids_germany(variable, from, to)

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

# read_shape_file --------------------------------------------------------------
read_shape_file <- function(
    file,
    target_crs = NULL,
    use_sf = FALSE,
    drop_z = FALSE,
    as_spatial = FALSE
)
{
  # Stop if this is not a shape file
  stopifnot(identical(tolower(kwb.utils::fileExtension(file)), "shp"))

  # Read the shape file
  shape <- if (use_sf) {
    sf::st_read(file)
  } else {
    rgdal::readOGR(file)
  }

  # Transform to coordinate reference system if given
  if (!is.null(target_crs)) {
    shape <- if (use_sf) {
      sf::st_transform(shape, target_crs)
    } else {
      sp::spTransform(shape, target_crs)
    }
  }

  # Drop z dimensions if desired
  if (drop_z) {
    shape <- sf::st_zm(shape)
  }

  # Convert to Spatial* object if desired
  if (as_spatial) {
    shape <- sf::as_Spatial(shape)
  }

  shape
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
