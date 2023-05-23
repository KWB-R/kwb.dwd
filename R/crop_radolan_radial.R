# crop_radolan_radial ----------------------------------------------------------

#' Crop Radolan Data
#'
#' @param radolan radolan data as returned by [read_relevant_years_radolan]
#' @param longitude longitude
#' @param latitude latitude
#' @param radius distance from point (latitude/longitude) in km
#' @param as_data_frame if `TRUE`, a data frame with columns
#'   `SAMPLE_DATE` and `rain_mean` is returned, otherwise the raster
#'   object returned by [raster::crop]
#' @export
#' @importFrom sf st_buffer st_transform
#' @importFrom raster cellStats crop
#' @importFrom lubridate ymd
#' @seealso
#'  * [get_radolan_urls],
#'  * [extract_radolan_zip_files],
#'  * [radolan_raw_to_raster],
#'  * [download_radolan].
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
#' Coordinates to EPSG-4326
#'
#' @param latitude latitude
#' @param longitude longitude
#'
#' @return data.frame with columns lon and lat with CRS 4326
#' @export
#' @importFrom sf st_as_sf
#' @examples
#' coordinates_to_EPSG_4326(latitude = 10, longitude = 50)
coordinates_to_EPSG_4326 <- function(latitude, longitude)
{
  sf::st_as_sf(
    x = data.frame(lon = longitude, lat = latitude),
    coords = c("lon", "lat"),
    crs = 4326
  )
}
