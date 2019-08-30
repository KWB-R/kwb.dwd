# crop_radolan_radial ----------------------------------------------------------

#' Crop Radolan Data
#'
#' @param radolan radolan data as returned by
#'   \code{\link{read_relevant_years_radolan}}
#' @param longitude longitude
#' @param latitude latitude
#' @param radius distance from point (latitude/longitude) in km
#' @param as_data_frame if \code{TRUE}, a data frame with columns
#'   \code{SAMPLE_DATE} and \code{rain_mean} is returned, otherwise the raster
#'   object returned by \code{\link[raster]{crop}}
#' @export
#'
crop_radolan_radial <- function(
  radolan, longitude, latitude, radius = 10, as_data_frame = TRUE
)
{
  # Apply radolan projection to location
  location_crs <- sf::st_transform(
    x = coordinates_to_EPSG_4326(latitude, longitude),
    crs = get_radolan_projection_string()
  )

  # Define buffer area around location with dist = 10 km (to be checked)
  location_buffer <- sf::st_buffer(location_crs, dist = radius)

  # Extract rain data for buffer area from radolan raster stack
  crop_location <- raster::crop(radolan, location_buffer)

  if (! as_data_frame) {

    return(crop_location)
  }

  # Calculate average rainfall of cropped raster
  x <- raster::cellStats(crop_location, mean) / 10

  data.frame(
    SAMPLE_DATE = lubridate::ymd(substr(names(x), 2, 7)),
    rain_mean = as.numeric(x)
  )
}

# coordinates_to_EPSG_4326 -----------------------------------------------------
coordinates_to_EPSG_4326 <- function(latitude, longitude)
{
  sf::st_as_sf(
    x = data.frame(lon = longitude, lat = latitude),
    coords = c("lon", "lat"),
    crs = 4326
  )
}
