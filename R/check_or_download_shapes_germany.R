# check_or_download_shapes_germany ---------------------------------------------

#' Check Local Availability or Download Shape Files
#'
#' This function checks if shape files for Germany are available in the folder
#' `\%TEMP\%/R_kwb.dwd/gadm40_DEU_shp`. If not, the shape files are downloaded
#' from \url{https://gadm.org} (URL to zip-file:
#' \url{https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip})
#' and unpacked into that folder.
#'
#' @param quiet passed if \code{TRUE} status messages are suppressed
#' @return path to folder containing shape files
#' @export
check_or_download_shapes_germany <- function(quiet = FALSE)
{
  url <- "https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip"

  # Path to sub folder below %TEMP% containing shape files for Germany
  shape_dir <- temp_dir(template. = url)

  # If the shape directory contains at least one .shp file, return the path to
  # the directory
  if (length(dir(shape_dir, "\\.shp$")) > 0L) {
    return(shape_dir)
  }

  # If <shape_dir> does not contain .shp files, download the zip-archive from
  # <url> and extract it into <shape_dir>
  file <- download_if_not_there(
    url,
    target_dir = download_dir("shapes"),
    quiet = quiet,
    mode = "wb"
  )

  # Extract the archive file into the shape directory
  unzip_zip_file(file, target_dir = shape_dir)

  shape_dir
}
