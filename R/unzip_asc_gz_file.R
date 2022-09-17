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
  args <- arg_list_file_or_url(file, url)

  # Read text lines from (remote or local) .gz file
  text <- do.call(read_lines_from_gz_file, args)

  target_name <- kwb.utils::removeExtension(basename(args[[1L]]))

  file <- file.path(target_dir, target_name)

  writeLines(text, file)

  file
}
