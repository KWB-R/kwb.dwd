# read_zipped_asc_file_at_url --------------------------------------------------

#' Read Values from .asc.gz File at URL
#'
#' @param url url
#' @return matrix with attributes containing metadata
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom utils read.table
read_zipped_asc_file_at_url <- function(url)
{
  # Download .gz file from URL, extract the file and read the lines as text
  text <- read_lines_from_downloaded_gz(url)

  # The first six rows are header rows
  header_indices <- 1:6

  # Extract the header
  header <- text[header_indices]

  # Extract the actual data values into a matrix
  values <- as.matrix(utils::read.table(text = text[-header_indices]))

  # Main arguments to structure() call
  args <- list(values, header = header)

  # Add header and further metadata as attributes
  do.call(structure, c(args, extract_metadata_from_url(url)))
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
