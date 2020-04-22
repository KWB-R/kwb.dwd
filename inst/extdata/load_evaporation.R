#
# Load evaporation data from DWD server
#
# Source the whole script first to load the function defined below
# Manually go through the commands within the MAIN section
#

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  # Base URL to potential evaporation files on DWD server
  base_url <- kwb.dwd:::ftp_path_cdc("grids_germany/monthly/evapo_p")

  # List data files
  relative_urls <- grep(
    "\\.asc\\.gz$", kwb.dwd::list_url(base_url), value = TRUE
  )

  # Provide full paths
  urls <- file.path(base_url, relative_urls)

  # Read all files into a list of matrices
  evaporation_matrices <- lapply(
    urls, kwb.dwd:::read_potential_evaporation_from_url
  )

  # Helper function to collect a specific attribute from all list elements
  collect <- function(x) sapply(evaporation_matrices, kwb.utils::getAttribute, x)

  # Provide metadata: file name, year, month
  file_info <- data.frame(
    file = relative_urls,
    year = collect("year"),
    month = collect("month")
  )

  head(file_info)

  str(evaporation_matrices[[1]])

  # Get Berlin matrix, same size as DWD evpo matrix (Berlin grid cells set to 1, rest of cells = NA)
  berlin_dwd_mask <- kwb.dwd:::get_berlin_dwd_mask()

  # calculate monthly stats for Berlin
  berlin_evap_monthly <- kwb.dwd:::calculate_potential_evaporation_stats(
    evaporation_matrices = evaporation_matrices,
    file_info = file_info,
    geo_mask = berlin_dwd_mask
  )
}
