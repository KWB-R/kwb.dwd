# read_shape_file --------------------------------------------------------------
#' @importFrom kwb.utils fileExtension
#' @importFrom rgdal readOGR
#' @importFrom sf as_Spatial st_read st_zm
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

  # Transform to target coordinate reference system if given
  shape <- transform_coords(shape, target_crs, use_sf)

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

#' # read_shape_file_2 ------------------------------------------------------------
#' #' @importFrom rgdal readOGR
#' read_shape_file_2 <- function(file)
#' {
#'   # TODO: Look here: https://r-spatial.org/r/2022/04/12/evolution.html
#'   rgdal::readOGR(
#'     dsn = file,
#'     stringsAsFactors = FALSE,
#'     encoding = "UTF-8",
#'     use_iconv = TRUE
#'   )
#' }
