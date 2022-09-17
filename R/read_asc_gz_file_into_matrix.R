# read_asc_gz_file_into_matrix -------------------------------------------------

#' Read Zipped File(s) in ESRI-ascii-grid-format at URL
#'
#' @param file path to downloaded file to be read
#' @param scale optional. Scaling factor by which to multiply each matrix value
#' @return matrix with attributes containing metadata
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom utils read.table
read_asc_gz_file_into_matrix <- function(file, scale = NULL)
{
  # Download .gz file from URL, extract the file and read the lines as text
  read_lines_from_gz_file(file) %>%
    read_esri_ascii_grid_lines_into_matrix(scale = scale)
}

# read_esri_ascii_grid_lines_into_matrix ---------------------------------------
read_esri_ascii_grid_lines_into_matrix <- function(text, scale = NULL)
{
  # The first six rows are header rows
  header_indices <- 1:6

  stopifnot(is.character(text), length(text) > max(header_indices))

  # Extract the actual data values into a matrix
  result <- as.matrix(utils::read.table(text = text[-header_indices]))

  if (! is.null(scale)) {
    result <- result * scale
  }

  # Add header and further metadata as attributes
  add_attributes(result, extract_metadata_from_header(text[header_indices]))
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

    file <- download_if_not_there(
      url,
      file.path(temp_dir(), basename(url)),
      quiet = TRUE
    )

  } else {

    assert_ending_gz(file)
    kwb.utils::safePath(file)
  }

  con <- gzfile(file, encoding = encoding)
  on.exit(close(con))
  readLines(con)
}
