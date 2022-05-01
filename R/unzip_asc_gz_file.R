# unzip_asc_gz_file ------------------------------------------------------------

#' Download a .gz File and Unzip it
#'
#' @param file path to asc.gz file to be unzipped
#' @param url optional. If given, the file to be unzipped is first downloaded
#'   from that URL
#' @param target_dir path to folder into which to (if applicable, download and)
#'   unzip the file
#' @return path to the unzipped file
#' @export
#' @importFrom kwb.utils removeExtension
unzip_asc_gz_file <- function(file, url = NULL, target_dir = tempdir())
{
  # Prepare arguments to read_lines_from_gz_file (either file or url)
  args <- if (is.null(url)) {
    list(file = file)
  } else {
    list(url = url)
  }

  path <- args[[1L]]

  # Read text lines from (remote or local) .gz file
  text <- do.call(read_lines_from_gz_file, args)

  file <- file.path(target_dir, kwb.utils::removeExtension(basename(path)))

  writeLines(text, file)

  file
}
