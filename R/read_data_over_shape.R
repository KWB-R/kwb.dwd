# read_monthly_data_over_shape -------------------------------------------------

#' Read monthly data from DWD, mask region with given shape file
#'
#' @inheritParams read_data_over_shape
#' @return data frame
#' @export
#' @seealso
#'  * [read_binary_radolan_file],
#'  * [read_asc_gz_file],
#'  * [read_relevant_years_radolan],
#'  * [read_daily_data_over_shape].
read_monthly_data_over_shape <- function(
    file = NULL,
    variable,
    from,
    to,
    quiet = FALSE,
    shape = NULL,
    use_sf = TRUE,
    ...
)
{
  read_data_over_shape(
    "monthly", file, variable, from, to, quiet, shape, use_sf, ...
  )
}

# read_daily_data_over_shape ---------------------------------------------------

#' Read daily data from DWD, mask region with given shape file
#'
#' Currently, only full months of data can be loaded, `from` and `to`
#' must be given
#'
#' @inheritParams read_data_over_shape
#' @return data frame
#' @export
#' @seealso
#'  * [read_monthly_data_over_shape],
#'  * [read_binary_radolan_file],
#'  * [read_asc_gz_file],
#'  * [read_relevant_years_radolan].
read_daily_data_over_shape <- function(file, variable, from, to, quiet = FALSE)
{
  read_data_over_shape(
    "daily", file, variable, from, to, quiet = quiet
  )
}

# read_data_over_shape ---------------------------------------------------------

#' Read data from DWD, mask region with given shape file or spatial object
#'
#' @param file path to shape file .shp
#' @param resolution one of "daily", "monthly"
#' @param variable currently, the following variables are supported: "evapo_p",
#'   "evapo_r", "frost_depth", "soil_moist", "soil_temperature_5cm" (daily),
#'   "evapo_p" (monthly)
#' @param from first month as "yyyymm" string
#' @param to last month as "yyyymm" string
#' @param quiet passed to [download.file]
#' @param shape object of class sf or sf_layers or SpatialPolygonsDataFrame.
#'   If `NULL` (the default), this object is obtained by calling
#'   `kwb.dwd:::read_shape_file` on `file`.
#' @param use_sf passed to `kwb.dwd:::read_shape_file` if applicable
#' @param \dots further arguments passed to `kwb.dwd:::read_shape_file`,
#'   such as `drop_z = TRUE`, if applicable
#' @return data frame
#' @export
read_data_over_shape <- function(
    resolution,
    file,
    variable,
    from,
    to,
    quiet = FALSE,
    shape = NULL,
    use_sf = TRUE,
    ...
)
{
  # Define scaling factors per variable. Depending on the variable, the values
  # in the data files need to be multiplied with a scaling factor.
  scales <- get_scaling_factors()

  variable <- match.arg(variable, names(scales))

  # Select the appropriate scaling factor
  scale <- kwb.utils::selectElements(scales, variable)

  # If no spatial object is given in shape and if a shape file is given, read
  # a spatial object from the shape file and transform it to the coordinate
  # system used in the grids provided by DWD
  if (is.null(shape) && !is.null(file)) {

    # Read shape file (and transform to projection used in DWD's grid files)
    shape <- if (resolution == "monthly") {

      read_shape_file(file, use_sf = use_sf, ...)

    } else if (resolution == "daily") {

      read_shape_with_dwd_projection(file)
    }
  }

  # Download (and extract) files from DWD server
  grid_files <- download_grids_germany(
    resolution, variable, from, to, quiet = quiet
  )

  # Read data within shape from all grid files
  get_data_from_grid_files(resolution, grid_files, shape, scale, use_sf)
}

# read_shape_with_dwd_projection -----------------------------------------------
read_shape_with_dwd_projection <- function(file, ...)
{
  # Read example grid from DWD, just to get its projection string
  example_grid <- get_example_grid_germany()

  # Read the shape file, transforming the projection to DWD's projection
  read_shape_file(file, target_crs = example_grid@crs, ...)
}

# get_data_from_grid_files -----------------------------------------------------
get_data_from_grid_files <- function(
    resolution, grid_files, shape, scale, use_sf
)
{
  # Define a suitable read function
  read <- if (resolution == "daily") {

    function(file) {
      # Provide a .prj file containing DWD's projection
      provide_projection_file(file)
      kwb.utils::catAndRun(paste("Reading", file), raster::raster(file))
    }

  } else if (resolution == "monthly") {

    read_asc_gz_file

  } else {

    clean_stop("resolution must be one of 'daily', 'monthly'.")
  }

  # Read the files into RasterLayer objects with DWD projection
  grids <- lapply(grid_files, read)

  # If a shape is given, mask the full grid over Germany with the shape and crop
  # the grid
  if (!is.null(shape) && length(grids)) {

    # Transform the spatial object to the coordinate system used in the grids
    shape <- transform_coords(
      shape,
      target_crs = grids[[1L]]@crs,
      use_sf = use_sf
    )

    grids <- mask_and_crop_grids(grids, shape)
  }

  # Provide file metadata (file, year, month[, day])
  metadata <- extract_metadata_from_files(
    files = grid_files,
    is_daily = (resolution == "daily")
  )

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
