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
  # Base URL to precipitation files on DWD server
  base_url <- kwb.dwd:::ftp_path_cdc("grids_germany/monthly/precipitation")

  # Get relative paths to all files in all sub folders
  paths <- kwb.dwd::list_url(base_url, recursive = TRUE)

  # Filter for .gz files, and create the full URLs
  urls <- file.path(base_url, grep("\\.gz$", paths, value = TRUE))

  # Calculate monthly stats for Berlin
  kwb.dwd:::calculate_potential_evaporation_stats(
    # Read all files into a list of matrices
    matrices = lapply(urls, kwb.dwd:::read_potential_evaporation_from_url),
    # Get Berlin matrix, same size as DWD evpo matrix (Berlin grid cells set to
    # 1, rest of cells = NA)
    geo_mask = kwb.dwd:::get_berlin_dwd_mask()
  )
}
