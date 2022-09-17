# unzip_asc_gz_file ------------------------------------------------------------

#' Download a .gz File and Unzip it
#'
#' @param file path to asc.gz file to be unzipped or URL from which the file is
#'   to be downloaded first
#' @param target_dir path to folder into which to (if applicable, download and)
#'   unzip the file
#' @return path to the unzipped file
#' @export
#' @importFrom kwb.utils removeExtension
unzip_asc_gz_file <- function(file, target_dir = tempdir())
{
  # Read text lines from (remote or local) .gz file
  text <- read_lines_from_gz_file(file)

  target_name <- kwb.utils::removeExtension(basename(file))

  file <- file.path(target_dir, target_name)

  writeLines(text, file)

  file
}
