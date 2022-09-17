# convert_bin_to_raster_file ---------------------------------------------------

#' Convert Binary Radolan File to Raster File
#'
#' @param bin_file path to binary radolan file or vector of such paths
#' @param target_dir directory in which to create the raster file(s). By default
#'   the files are created in a subfolder "rst" parallel to the folder in which
#'   the first element of \code{bin_file} resides.
#' @param dbg if \code{TRUE}, debug messages are shown
#' @export
#' @importFrom kwb.utils createDirectory
convert_bin_to_raster_file <- function(bin_file, target_dir = NULL, dbg = TRUE)
{
  stopifnot(is.character(bin_file))

  if (length(bin_file) == 0) {

    message("Nothing to convert.")
    return()
  }

  # Default target directory: <bin_file_dir>/../rst/
  if (is.null(target_dir)) {

    target_dir <- kwb.utils::createDirectory(file.path(
      dirname(dirname(bin_file[1])), "rst"
    ))
  }

  # Call this function for each file if more than one file is given
  if (length(bin_file) > 1) {

    return(invisible(lapply(
      X = bin_file,
      FUN = convert_bin_to_raster_file,
      target_dir = target_dir,
      dbg = dbg
    )))
  }

  stopifnot(file.exists(bin_file))

  # Read binary file into raster object
  rb <- read_binary_radolan_file(bin_file)

  # Determine target file name
  file <- file.path(target_dir, gsub("---bin$", "---rst", basename(bin_file)))

  # Write raster data to file
  write_raster_to_file(rb, file = file, dbg = dbg)
}

#' Write raster to file
#'
#' @param rb raster data
#' @param file file path
#' @param dbg debug (TRUE/FALSE)
#'
#' @return write raster to file
#' @export
#'
#' @importFrom raster writeRaster
write_raster_to_file <- function(rb, file, dbg = TRUE)
{
  kwb.utils::catAndRun(
    messageText = paste("Saving raster object to", file),
    dbg = dbg,
    expr = raster::writeRaster(rb, filename = file, overwrite = TRUE)
  )
}
