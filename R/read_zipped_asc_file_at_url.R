# read_zipped_esri_ascii_grid --------------------------------------------------

#' Read Zipped File(s) in ESRI-ascii-grid-format at URL
#'
#' @param url URL to file
#' @param scale optional. Scaling factor by which to multiply each matrix value
#' @return matrix with attributes containing metadata
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom utils read.table
read_zipped_esri_ascii_grid <- function(url, scale = NULL)
{
  #url <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/evapo_p/grids_germany_monthly_evapo_p_202203.asc.gz"

  # Download .gz file from URL, extract the file and read the lines as text
  text <- read_lines_from_downloaded_gz(url)

  # The first six rows are header rows
  header_indices <- 1:6

  # Extract the actual data values into a matrix
  result <- as.matrix(utils::read.table(text = text[-header_indices]))

  if (! is.null(scale)) {
    result <- result * scale
  }

  # Add header and further metadata as attributes
  do.call(structure, c(
    list(result),
    extract_metadata_from_header(text[header_indices]),
    extract_metadata_from_url(url)
  ))
}

# extract_metadata_from_header -------------------------------------------------
extract_metadata_from_header <- function(header_lines)
{
  cells <- strsplit(header_lines, "\\s+")

  values <- as.numeric(sapply(cells, "[[", 2L))

  stats::setNames(as.list(values), sapply(cells, "[[", 1L))
}

# read_lines_from_downloaded_gz ------------------------------------------------
read_lines_from_downloaded_gz <- function(url)
{
  stopifnot(is.character(url), length(url) == 1L)

  file <- file.path(tempdir(), basename(url))

  download.file(url, file, method = "auto")

  con <- gzfile(file)

  on.exit(close(con))

  readLines(con)
}

# extract_metadata_from_url ----------------------------------------------------
#' @importFrom kwb.utils extractSubstring
extract_metadata_from_url <- function(url)
{
  name <- basename(url)

  year_month <- kwb.utils::extractSubstring("(\\d{4})(\\d{2})", name, 1:2)

  list(
    year = as.integer(year_month[[1L]]),
    month = as.integer(year_month[[2L]]),
    file = name,
    origin = dirname(url)
  )
}
