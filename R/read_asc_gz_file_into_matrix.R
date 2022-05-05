# read_asc_gz_file_into_matrix -------------------------------------------------

#' Read Zipped File(s) in ESRI-ascii-grid-format at URL
#'
#' @param url URL to file
#' @param scale optional. Scaling factor by which to multiply each matrix value
#' @return matrix with attributes containing metadata
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom utils read.table
read_asc_gz_file_into_matrix <- function(url, scale = NULL)
{
  #url <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/evapo_p/grids_germany_monthly_evapo_p_202203.asc.gz"

  # Download .gz file from URL, extract the file and read the lines as text
  text <- read_lines_from_gz_file(url = url)

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
    extract_metadata_from_urls(url)
  ))
}

# extract_metadata_from_header -------------------------------------------------
#' @importFrom stats setNames
extract_metadata_from_header <- function(header_lines)
{
  cells <- strsplit(header_lines, "\\s+")

  values <- as.numeric(sapply(cells, "[[", 2L))

  stats::setNames(as.list(values), sapply(cells, "[[", 1L))
}

# read_lines_from_gz_file ------------------------------------------------------
#' @importFrom kwb.utils safePath
read_lines_from_gz_file <- function(
  file, url = NULL, encoding = getOption("encoding")
)
{
  # If URL is given, download file from URL to temporary directory
  if (! is.null(url)) {

    assert_url(url, final_slash = FALSE)
    assert_ending_gz(url)

    file <- file.path(temp_dir(), basename(url))

    download_if_not_there(url, file)

  } else {

    assert_ending_gz(file)
    kwb.utils::safePath(file)
  }

  con <- gzfile(file, encoding = encoding)
  on.exit(close(con))
  readLines(con)
}

# download_if_not_there --------------------------------------------------------
download_if_not_there <- function(url, file = file.path(tempdir(), basename(url)))
{
  if (file.exists(file)) {
    cat("File already available:", file, "\n")
  } else {
    download.file(url, file, method = "auto")
  }

  file
}
