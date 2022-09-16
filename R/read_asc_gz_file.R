# read_asc_gz_file -------------------------------------------------------------

#' Read Zipped ESRI-Ascii-Grid File (from URL)
#'
#' @param file path to zipped file in ESRI-ascii-grid format (.asc.gz)
#' @param url optional. URL to zipped file in ESRI-ascii-grid format (.asc.gz)
#' @return object of class "RasterLayer"
#' @export
#' @importFrom kwb.utils callWith removeExtension tempSubdirectory
#' @importFrom raster raster
read_asc_gz_file <- function(file, url = NULL)
{
  target_dir <- if (is.null(url)) {
    dirname(file)
  } else {
    temp_dir(template. = kwb.utils::removeExtension(url))
  }

  # Call the unzip function setting either "url" or "file" argument
  grid_file <- kwb.utils::callWith(
    FUN = unzip_asc_gz_file,
    if (is.null(url)) list(file = file) else list(url = url),
    target_dir = target_dir
  )

  #dir(target_dir)

  # Provide a copy of the projection file in the download folder
  provide_projection_file(grid_file)

  raster::raster(grid_file)
}
