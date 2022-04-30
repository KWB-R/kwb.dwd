# download_gz_file_and_unzip ---------------------------------------------------

#' Download a .gz File and Unzip it
#'
#' @param URL URL to .gz file
#' @param download_dir path to folder into which to download and unzip the file
#' @return path to the unzipped file
#' @export
download_gz_file_and_unzip <- function(url, download_dir = tempdir())
{
  stopifnot(grepl("\\.gz$", url))

  # Download .gz file from URL, extract the file and read the lines as text
  text <- read_lines_from_downloaded_gz(url)

  file <- file.path(download_dir, kwb.utils::removeExtension(basename(url)))

  writeLines(text, file)

  file
}
