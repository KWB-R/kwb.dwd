# read_monthly_data_over_shape -------------------------------------------------

#' Read monthly data from DWD, mask region with given shape file
#'
#' @param file path to shape file .shp
#' @param variable currently only "evapo_p" is supported
#' @param from first month as "yyyymm" string
#' @param to last month as "yyyymm" string
#' @param quiet passed to \code{\link{download.file}}
#' @param \dots further arguments passed to \code{kwb.dwd:::read_shape_file},
#'   such as \code{drop_z = TRUE, use_sf = TRUE}
#' @return data frame
#' @export
read_monthly_data_over_shape <- function(file, variable, from, to, quiet = FALSE, ...)
{
  # Define scaling factors per variable. Depending on the variable, the values
  # in the data files need to be multiplied with a scaling factor.
  scales <- get_scaling_factors()

  variable <- match.arg(variable, names(scales))

  # Select the appropriate scaling factor
  scale <- kwb.utils::selectElements(scales, variable)

  # Read shape file and transform to projection used in DWD's grid files
  shape <- read_shape_with_dwd_projection(file, ...)

  # Download files from DWD server
  grid_files <- download_monthly_grids_germany(variable, from, to, quiet = quiet)

  # Read the files into RasterLayer objects with DWD projection
  grids <- lapply(grid_files, read_asc_gz_file)

  # Mask the full grid over Germany with the shape and crop the grid
  grids <- mask_and_crop_grids(grids, shape)

  # Calculate statistics, considering the conversion factor "scale"
  data <- do.call(rbind, lapply(grids, raster_stats, scale = scale))

  # Provide file metadata (file, year, month)
  metadata <- extract_metadata_from_files_monthly(files = grid_files)

  # Add metadata
  cbind(metadata, data)
}
