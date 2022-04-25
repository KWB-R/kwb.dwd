#
# Load potential evaporation data from DWD server
#
# Source the whole script first to load the function defined below (if any)
# Manually go through the commands within the MAIN section
#

# MAIN -------------------------------------------------------------------------
load_potential_evaporation_berlin <- function()
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
  matrices <- lapply(urls, kwb.dwd:::read_potential_evaporation_from_url)

  str(matrices[[1]])

  # Get Berlin matrix, same size as DWD evpo matrix (Berlin grid cells set to 1, rest of cells = NA)
  berlin_dwd_mask <- kwb.dwd:::get_berlin_dwd_mask()

  # calculate monthly stats for Berlin
  # use "monthly_evapo_p" because the filename at DWD reads the same
  result <- kwb.dwd:::calculate_potential_evaporation_stats(
    matrices = matrices,
    geo_mask = berlin_dwd_mask
  )

  # Provide metadata: file name, year, month
  file_info <- data.frame(
    file = relative_urls,
    year = sapply(matrices, kwb.utils::getAttribute, "year"),
    month = sapply(matrices, kwb.utils::getAttribute, "month")
  )

  structure(result, file_info = file_info)
}
