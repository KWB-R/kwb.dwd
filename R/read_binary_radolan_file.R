# read_binary_radolan_file -----------------------------------------------------

#' Read Binary Radolan File
#'
#' @param path path to binary Radolan file
#' @param set_projection_and_extent if `TRUE` (default), the projection
#'   and extent of the raster object are set to the Radolan specific values,
#'   returned by `kwb.dwd::get_radolan_projection_string()` and
#'   `kwb.dwd::get_radolan_extension()`, respectively.
#' @param consider_flags logical. Should the flags be considered? If
#'   `TRUE`, values where the "invalid" or "clutter" flag is set are set to
#'   `NA` and values where the "negative" flag is set are negated. The
#'   default is `TRUE` (for compatibility reasons) but this should change
#'   in future!
#' @export
#' @importFrom sp CRS
#' @importFrom raster extent
read_binary_radolan_file <- function(
    path,
    set_projection_and_extent = TRUE,
    consider_flags = FALSE
)
{
  # Read raw data values
  rbi <- read_binary_radolan_file_raw(path)

  # Consider flags, set e.g. invalid values to NA if requested
  if (consider_flags) {
    rbi <- consider_flags(rbi)
  }

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

# consider_flags ---------------------------------------------------------------
#' @importFrom kwb.utils getAttribute
consider_flags <- function(x, flags = kwb.utils::getAttribute(x, "flags"))
{
  bit_is_set <- function(x, bit) bitwAnd(x, 2^(bit - 1L)) > 0L

  is_invalid <- bit_is_set(flags, 14L)
  is_negative <- bit_is_set(flags, 15L)
  is_clutter <- bit_is_set(flags, 16L)

  x[is_negative] <- -x[is_negative]
  x[is_invalid | is_clutter] <- NA

  x
}
