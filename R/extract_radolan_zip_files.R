# extract_radolan_zip_files ----------------------------------------------------

#' Extract radolan zip files
#'
#' @param radolan_dir path to directory containing zipped Radolan files (ending
#'   in .tar.gz)
#'
#' @return Nothing. Side effect: Unzipped files in subfolder "bin" below
#'   \code{radolan_dir}.
#'
#' @export
#' @importFrom kwb.utils catAndRun createDirectory
#' @importFrom utils untar
extract_radolan_zip_files <- function(radolan_dir)
{
  # Get the paths to the zipped files
  pattern <- "\\.tar\\.gz"
  zip_files <- dir(radolan_dir, pattern, recursive = TRUE, full.names = TRUE)

  # Get the names of the binary files contained in the zip archives
  expected_bins <- list_files_in_zip_files(zip_files)

  # Directory of extracted binary files
  export_dir <- kwb.utils::createDirectory(file.path(radolan_dir, "bin"))

  # Determining files that need to be extracted
  to_be_extracted <- kwb.utils::catAndRun(
    messageText = "Determining files that need to be extracted", {

      # Get the paths to all binary Radolan files in the export directory
      existing_bins <- dir(export_dir, "---bin$", full.names = TRUE)

      # Have the binary files already been extracted?
      bin_exists <- expected_bins$file %in% basename(existing_bins)

      # Filter for the files that need to be extracted
      expected_bins[! bin_exists, ]
    }
  )

  # Extract all those files that do not yet exist in the export directory
  for (zip_file in unique(to_be_extracted$zip_file)) {

    zip_file_path <- zip_files[basename(zip_files) == zip_file]

    files <- to_be_extracted$file[to_be_extracted$zip_file == zip_file]

    stopifnot(length(zip_file_path) == 1)

    kwb.utils::catAndRun(
      messageText = sprintf(
        "Unzipping %d files from %s to %s", length(files), zip_file, export_dir
      ),
      expr = utils::untar(zip_file_path, files = files, exdir = export_dir)
    )
  }
}
