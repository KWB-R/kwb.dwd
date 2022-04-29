#' Load monthly potential evaporation for Berlin from DWD
#'
#' This function loads monthly potential evaporation for Berlin, Germany,
#' from Deutscher Wetterdienst (DWD).
#'
#' @return data frame with columns \code{file} (name of file downloaded from
#'   DWD), \code{year} (year number as integer), \code{month number as integer},
#'   \code{mean} (mean value), \code{sd} (standard deviation), \code{min}
#'   (minimum value), \code{max} (maximum value) of potential evaporation
#'   calculated for Berlin, Germany
#' @export
load_potential_evaporation_berlin <- function(...)
{
  load_monthly_variable_berlin(variable = "evapo_p", scale = 0.1, ...)
}

#' Load monthly precipitation for Berlin from DWD
#'
#' This function loads monthly precipitation for Berlin, Germany,
#' from Deutscher Wetterdienst (DWD).
#'
#' @return data frame with columns \code{file} (name of file downloaded from
#'   DWD), \code{year} (year number as integer), \code{month number as integer},
#'   \code{mean} (mean value), \code{sd} (standard deviation), \code{min}
#'   (minimum value), \code{max} (maximum value) of precipitation
#'   calculated for Berlin, Germany
#' @export
load_precipitation_berlin <- function(...)
{
  load_monthly_variable_berlin(variable = "precipitation", ...)
}

# load_monthly_variable_berlin -------------------------------------------------
load_monthly_variable_berlin <- function(
  variable, scale = NULL, from = NULL, to = NULL
)
{
  # Currently, two variable are supported
  match.arg(variable, c("precipitation", "evapo_p"))

  # URLs to .asc.gz files with monthly precipitation data DWD server
  urls <- list_zipped_esri_ascii_grids(
    ftp_path_monthly_grids(variable), from, to
  )

  # Read all files into a list of matrices
  matrices <- lapply(urls, read_zipped_esri_ascii_grid, scale = scale)

  # Calculate monthly stats for Berlin
  calculate_masked_grid_stats(matrices, geo_mask = get_berlin_dwd_mask())
}
