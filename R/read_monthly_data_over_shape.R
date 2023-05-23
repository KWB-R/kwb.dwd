# read_monthly_data_over_shape -------------------------------------------------

#' Read monthly data from DWD, mask region with given shape file
#'
#' @param file path to shape file .shp
#' @param variable currently only "evapo_p" is supported
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
    shape <- read_shape_file(file, use_sf = use_sf, ...)
  }

  # Download files from DWD server
  grid_files <- download_monthly_grids_germany(
    variable, from, to, quiet = quiet
  )

  # Read the files into RasterLayer objects with DWD projection
  grids <- lapply(grid_files, read_asc_gz_file)

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

  # Provide file metadata (file, year, month)
  metadata <- extract_metadata_from_files(files = grid_files, is_daily = FALSE)

  # Calculate statistics, considering the scaling factor, add metadata
  cbind(metadata, summarise_over_all_grids(grids, scale))
}
