#' Load precipitation data from DWD server for Berlin area
#'
#' This function loads monthly precipitation values for Berlin, Germany,
#' from Deutscher Wetterdienst (DWD).
#'
#' @return data frame with columns \code{file} (name of file downloaded from
#'   DWD), \code{year} (year number as integer), \code{month number as integer},
#'   \code{mean} (mean value), \code{sd} (standard deviation), \code{min}
#'   (minimum value), \code{max} (maximum value) of potential evaporation
#'   calculated for Berlin, Germany
#' @export
load_precipitation_berlin <- function()
{
  # URLs to .asc.gz files with monthly precipitation data DWD server
  urls <- list_zipped_esri_ascii_grids(ftp_path_monthly_grids("precipitation"))

  # Read all files into a list of matrices
  matrices <- lapply(urls, read_zipped_esri_ascii_grid)

  # Get Berlin matrix, same dimension as each of matrices (Berlin grid cells set
  # to 1, rest of grid cells = NA)
  geo_mask <- get_berlin_dwd_mask()

  # Calculate monthly stats for Berlin
  calculate_masked_grid_stats(matrices, geo_mask)
}
