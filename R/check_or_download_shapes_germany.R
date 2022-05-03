# check_or_download_shapes_germany ---------------------------------------------

#' Check Local Availability or Download Shape Files
#'
#' This function checks if shape files for Germany are available in the folder
#' `\%TEMP\%/R_kwb.dwd/gadm40_DEU_shp`. If not, the shape files are downloaded
#' from \url{https://gadm.org} (URL to zip-file:
#' \url{https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip})
#' and unpacked into that folder.
#'
#' @return path to folder containing shape files
#' @export
#' @importFrom archive archive_extract
#' @importFrom kwb.utils createDirectory removeExtension
check_or_download_shapes_germany <- function()
{
  url <- "https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip"

  zip_name <- basename(url)
  shape_name <- kwb.utils::removeExtension(zip_name)

  # Path to sub folder below %TEMP% containing shape files for Germany
  shape_dir <- temp_dir(shape_name)

  # If <shape_dir> does not exist, create it, download the shape files as
  # zip-archive from <url> and extract them into <shape_dir>
  if (! dir.exists(shape_dir)) {
    destfile <- file.path(tempdir(), zip_name)
    download.file(url, destfile)
    archive::archive_extract(destfile, kwb.utils::createDirectory(shape_dir))
  }

  shape_dir
}
