#
# Load evaporation data from DWD server
#
# Source the whole script first to load the function defined below
# Manually go through the commands within the MAIN section
#

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  # Base URL to evaporation files on DWD server
  base_url <- kwb.dwd:::ftp_path_cdc("grids_germany/monthly/evapo_p")
  
  # List data files
  relative_urls <- grep(
    "\\.asc\\.gz$", kwb.dwd::list_url(base_url), value = TRUE
  )
  
  # Provide full paths
  urls <- file.path(base_url, relative_urls)

  # Read all files into a list of matrices  
  evaporation_matrices <- lapply(urls, read_evaporation_matrix_from_url)

  # Provide metadata: file name, year, month  
  file_info <- data.frame(
    file = relative_urls,
    year = sapply(evaporation_matrices, kwb.utils::getAttribute, "year"),
    month = sapply(evaporation_matrices, kwb.utils::getAttribute, "month")
  )
  
  head(file_info)
  
  str(evaporation_matrices[[1]])
}

# read_evaporation_matrix_from_url ---------------------------------------------
read_evaporation_matrix_from_url <- function(url)
{
  stopifnot(is.character(url), length(url) == 1L)
  
  file_name <- basename(url)
  
  file <- file.path(tempdir(), file_name)

  download.file(url, file)
  
  con <- gzfile(file)
  
  on.exit(close(con))
  
  text <- readLines(con)

  year_month <- kwb.utils::extractSubstring("(\\d{4})(\\d{2})", file_name, 1:2)
  
  extract_date_part <- function(i) as.integer(year_month[[i]])
  
  structure(
    as.matrix(read.table(text = text[-(1:6)])),
    header = text[1:6],
    year = extract_date_part(1L),
    month = extract_date_part(2L)
  )
}
