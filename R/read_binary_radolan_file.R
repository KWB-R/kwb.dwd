# read_binary_radolan_file -----------------------------------------------------

#' Read Binary Radolan File
#'
#' @param path path to binary Radolan file
#' @param set_projection_and_extent if \code{TRUE} (default), the projection
#'   and extent of the raster object are set to the Radolan specific values,
#'   returned by \code{kwb.dwd::get_radolan_projection_string()} and
#'   \code{kwb.dwd::get_radolan_extension()}, respectively.
#' @export
#' @importFrom sp CRS
#' @importFrom raster extent
read_binary_radolan_file <- function(path, set_projection_and_extent = TRUE)
{
  # Read raw data values
  rbi <- read_binary_radolan_file_raw(path)

  # Convert raw data values to raster
  rb <- radolan_raw_to_raster(rbi)

  # Set projection and extent
  if (set_projection_and_extent) {

    raster::projection(rb) <- sp::CRS(get_radolan_projection_string())
    raster::extent(rb) <- raster::extent(get_radolan_extension())
  }

  # Return the raster object
  rb
}

#' Get Radolan Extension
#'
#' @return default radolan extension
#' @export
#'
#' @examples
#' get_radolan_extension()
#'
get_radolan_extension <- function()
{
  c(-523.4622, 376.5378, -4658.645, -3758.645)
}
