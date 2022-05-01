# read_asc_gz_file -------------------------------------------------------------

#' Read Zipped ESRI-Ascii-Grid File (from URL)
#'
#' @param file path to zipped file in ESRI-ascii-grid format (.asc.gz)
#' @param url optional. URL to zipped file in ESRI-ascii-grid format (.asc.gz)
#' @return object of class "RasterLayer"
#' @export
read_asc_gz_file <- function(file, url = NULL)
{
  target_dir <- if (is.null(url)) {
    dirname(file)
  } else {
    re <- kwb.utils::removeExtension
    kwb.utils::tempSubdirectory(re(re(basename(url))))
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

# provide_projection_file ------------------------------------------------------
provide_projection_file <- function(file)
{
  destfile <- file.path(
    dirname(file),
    replace_file_extension(basename(file), ".prj")
  )

  if (file.exists(destfile)) {
    message("There is already a projection file: ", destfile)
    return()
  }

  download.file(url_projection(), destfile, method = "auto")
}

# url_projection ---------------------------------------------------------------
url_projection <- function()
{
  "https://opendata.dwd.de/climate_environment/CDC/help/gk3.prj"
}

# replace_file_extension -------------------------------------------------------
replace_file_extension <- function(file, extension)
{
  paste0(kwb.utils::removeExtension(file), extension)
}
