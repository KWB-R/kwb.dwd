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

  # Path to sub folder below %TEMP% containing shape files for Germany
  shape_dir <- temp_dir(template. = url, create. = FALSE)

  if (dir.exists(shape_dir)) {
    return(shape_dir)
  }

  # If <shape_dir> does not exist, create it, download the zip-archive from
  # <url> and extract it into <shape_dir>
  archive::archive_extract(download_if_not_there(url), shape_dir)

  shape_dir
}
