# crop_radolan_radial ----------------------------------------------------------

#' Crop Radolan Data
#'
#' @param radolan radolan data as returned by
#'   \code{\link{read_relevant_years_radolan}}
#' @param longitude longitude
#' @param latitude latitude
#' @param radius distance from point (latitude/longitude) in km
#' @export
#'
crop_radolan_radial <- function(radolan, longitude, latitude, radius = 10)
{
  coord <- data.frame(lat = latitude, lon = longitude)

  # Transform lat/lon input to st object
  location <- sf::st_as_sf(coord, coords = c("lon", "lat"), crs = 4326)

  # Define coordinate reference system for Radolan data
  reference_system <- raster::crs(asText = TRUE, sp::CRS(
    get_radolan_projection_string()
  ))

  # Apply radolan projection to location
  location_crs <- sf::st_transform(x = location, crs = reference_system)

  # Define buffer area around locationdist = 10 km (to be checked)
  location_buffer <- sf::st_buffer(location_crs, dist = radius)

  # Extract rain data for buffer area from radolan raster stack
  crop_location <- raster::crop(radolan, location_buffer)

  # Calculate average rainfall of cropped raster
  x <- raster::cellStats(crop_location, mean) / 10

  data.frame(
    SAMPLE_DATE = lubridate::ymd(substr(names(x), 2, 7)),
    rain_mean = as.numeric(x)
  )
}
