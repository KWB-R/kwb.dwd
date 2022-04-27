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

  # Get all folders, each representing one month on DWD server
  monthly_folders <- kwb.dwd::list_url(base_url)[0:12]

  # Loop for all month-folders on DWD server
  for (month in monthly_folders)
  {
    # Get the url for each month
    monthly_url = paste(base_url, '/', month, sep="")

    # List data files
    relative_urls <- grep("\\.asc\\.gz$", kwb.dwd:::list_url(monthly_url), value = TRUE)

    # Provide full paths
    urls_month <- file.path(monthly_url, relative_urls)

    # Merge all monthly data to a single list
    if (exists('urls_all_months'))
    {
      urls_all_months <- append(urls_all_months, urls_month)
    }
    else
    {
      urls_all_months <- urls_month
    }

  }


  # Calculate monthly stats for Berlin
  kwb.dwd:::calculate_potential_evaporation_stats(
    # Read all files into a list of matrices
    matrices = lapply(urls_all_months, kwb.dwd:::read_potential_evaporation_from_url),
    # Get Berlin matrix, same size as DWD evpo matrix (Berlin grid cells set to
    # 1, rest of cells = NA)
    geo_mask = kwb.dwd:::get_berlin_dwd_mask()
  )
}
