# check_shapes_germany ---------------------------------------------------------
#' @importFrom archive archive_extract
#' @importFrom kwb.utils createDirectory removeExtension
check_shapes_germany <- function()
{
  url <- "https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip"

  zip_name <- basename(url)
  shape_name <- kwb.utils::removeExtension(zip_name)
  shape_dir <- file.path(Sys.getenv("TEMP"), "R_kwb.dwd", shape_name)

  if (! dir.exists(shape_dir)) {
    destfile <- file.path(tempdir(), zip_name)
    download.file(url, destfile)
    archive::archive_extract(destfile, kwb.utils::createDirectory(shape_dir))
  }

  shape_dir
}
