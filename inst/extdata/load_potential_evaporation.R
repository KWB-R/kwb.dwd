#
# Load potential evaporation data from DWD server
#
# Source the whole script first to load the function defined below (if any)
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
  matrices <- lapply(urls, kwb.dwd:::read_potential_evaporation_from_url)

  # Provide metadata from matrices' attributes: file name, year, month
  file_info <- as.data.frame(lapply(
    X = stats::setNames(nm = c("file", "year", "month")),
    FUN = function(x) sapply(matrices, kwb.utils::getAttribute, x)
  ))

  head(file_info)

  str(matrices[[1]])

  # Get Berlin matrix, same size as DWD evpo matrix (Berlin grid cells set to 1, rest of cells = NA)
  berlin_dwd_mask <- kwb.dwd:::get_berlin_dwd_mask()

  # calculate monthly stats for Berlin
  # use "monthly_evapo_p" because the filename at DWD reads the same
  berlin_monthly_evapo_p <- kwb.dwd:::calculate_potential_evaporation_stats(
    evaporation_matrices = matrices,
    file_info = file_info,
    geo_mask = berlin_dwd_mask
  )
}
