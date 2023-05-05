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
  # Path to the target file
  target <- file.path(target_dir, remove_right(basename(file), nchar(".gz")))

  # Read text lines from (remote or local) .gz file and write to target file
  writeLines(read_lines_from_gz_file(file), target)

  # Return the path to the created file
  target
}

# unzip_tar_file ---------------------------------------------------------------
unzip_tar_file <- function(file, target_dir)
{
  relative_paths <- utils::untar(file, list = TRUE)

  utils::untar(file, exdir = target_dir)

  # Return the paths to the extracted files
  file.path(target_dir, relative_paths)
}

# unzip_tar_gz_file ------------------------------------------------------------
unzip_tar_gz_file <- function(file, target_dir)
{
  stopifnot(length(file) == 1L)
  stopifnot(file.exists(file))
  stopifnot(endsWith(file, ".tar.gz"))

  kwb.utils::safePath(target_dir)

  # Get the names of the files contained in the zip archive
  zipped_files <- list_zipped_files(file)

  # Relative paths to existing files in export directory
  existing_files <- dir(target_dir, recursive = TRUE)

  # Files in the zip file that need to be extracted
  files_to_extract <- setdiff(zipped_files, existing_files)

  # Paths to all unzipped files
  target_files <- file.path(target_dir, zipped_files)

  if (length(files_to_extract) == 0L) {

    writeLines(sprintf(
      "\nNothing to do. All files in\n  %s\nare already available in\n  %s.",
      file,
      target_dir
    ))

    return(target_files)
  }

  # Extract all those files that do not yet exist in the export directory
  kwb.utils::catAndRun(
    messageText = sprintf(
      "\nUnzipping %d files from\n  %s\nto\n  %s",
      length(files_to_extract),
      file,
      target_dir
    ),
    expr = utils::untar(
      file,
      files = files_to_extract,
      exdir = target_dir
    )
  )

  # Return the paths to the unzipped files
  target_files
}

# unzip_zip_file ---------------------------------------------------------------
unzip_zip_file <- function(file, target_dir)
{
  archive::archive_extract(file, dir = target_dir)
}
