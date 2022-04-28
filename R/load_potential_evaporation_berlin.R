#' Load potential evaporation data from DWD server
#'
#' This function loads monthly potential evaporation values for Berlin, Germany,
#' from Deutscher Wetterdienst (DWD).
#'
#' @return data frame with columns \code{file} (name of file downloaded from
#'   DWD), \code{year} (year number as integer), \code{month number as integer},
#'   \code{mean} (mean value), \code{sd} (standard deviation), \code{min}
#'   (minimum value), \code{max} (maximum value) of potential evaporation
#'   calculated for Berlin, Germany
#' @export
load_potential_evaporation_berlin <- function()
{
  # URLs to .asc.gz files with potential evaporation data on DWD server
  urls <- list_zipped_asc_files(ftp_path_cdc("grids_germany/monthly/evapo_p"))

  # Calculate monthly stats for Berlin
  calculate_potential_evaporation_stats(
    # Read all files into a list of matrices
    matrices = lapply(urls, read_zipped_esri_ascii_grid),
    # Get Berlin matrix, same size as DWD evpo matrix (Berlin grid cells set to
    # 1, rest of cells = NA)
    geo_mask = get_berlin_dwd_mask()
  )
}
