if (FALSE)
{
  url <- "https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip"
  url <- "https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/germany-shapefile/at_download/file/Germany_shapefile.zip"
  url <- "https://www.arcgis.com/sharing/rest/content/items/ae25571c60d94ce5b7fcbf74e27c00e0/data/vg2500_geo84.zip"

  RCurl::url.exists(url)

  download.file(url, file.path(tempdir(), basename(url)), mode = "wb")
}

# check_or_download_shapes_germany ---------------------------------------------

#' Check Local Availability or Download Shape Files
#'
#' This function checks if shape files for Germany are available in the folder
#' \code{\%TEMP\%/R_kwb.dwd/shapes_germany}. If not, the shape files are
#' downloaded from the URL given in \code{url} and unpacked into that folder.
#'
#' @param url URL to publicly available zip file containing shape files for
#'   Germany. Default:
#'   \url{https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip}
#' @param quiet passed if \code{TRUE} status messages are suppressed
#' @param timeout timeout in seconds
#' @return path to folder containing shape files
#' @export
check_or_download_shapes_germany <- function(
    url = "https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip",
    quiet = FALSE,
    timeout = 60
)
{
  #kwb.utils::assignPackageObjects("kwb.dwd");quiet=FALSE;timeout=1

  # Path to sub folder below %TEMP% containing shape files for Germany
  shape_dir <- temp_dir("shapes_germany")

  # If the shape directory contains at least one .shp file, return the path to
  # the directory
  if (contains_file(shape_dir, pattern = "\\.shp$")) {
    return(shape_dir)
  }

  # If <shape_dir> does not contain .shp files, download the zip-archive from
  # <url> and extract it into <shape_dir>
  file <- try(silent = TRUE, suppressWarnings(download_if_not_there(
    url,
    target_dir = download_dir("shapes_germany"),
    quiet = quiet,
    mode = "wb",
    timeout = timeout
  )))

  if (kwb.utils::isTryError(file)) {
    clean_stop(as.character(file))
  }

  stopifnot(file.exists(file), file.exists(shape_dir))

  # Extract the archive file into the shape directory
  unzip_zip_file(file, target_dir = shape_dir)

  shape_dir
}
