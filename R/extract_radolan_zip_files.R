# extract_radolan_zip_files ----------------------------------------------------

#' Extract radolan zip files
#'
#' @param radolan_dir path to directory containing zipped Radolan files (ending
#'   in .tar.gz)
#'
#' @return Nothing. Side effect: Unzipped files in subfolder "bin" below
#'   `radolan_dir`.
#'
#' @export
#' @importFrom kwb.utils createDirectory
#' @seealso
#'  * [list_extracted_radolan_files].
extract_radolan_zip_files <- function(radolan_dir)
{
  #radolan_dir = download_dir("dwd")

  # Get paths to .tar.gz files below <radolan_dir>
  files <- dir(radolan_dir, "\\.tar\\.gz$", recursive = TRUE, full.names = TRUE)

  # Define and create target directory: <radolan_dir>/bin
  target_dir <- kwb.utils::createDirectory(file.path(radolan_dir, "bin"))

  # Unzip files to target directory, invisibly return paths to extracted files
  invisible(unlist(lapply(files, unzip_tar_gz_file, target_dir = target_dir)))
}
