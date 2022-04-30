# read_zipped_esri_ascii_grid_from_url -----------------------------------------

#' Read Zipped ESRI-Ascii-Grid File from URL
#'
#' @param url URL to zipped file in ESRI-ascii-grid format
#' @return object of class "RasterLayer"
#' @export
read_zipped_esri_ascii_grid_from_url <- function(url)
{
  re <- kwb.utils::removeExtension
  download_dir <- kwb.utils::tempSubdirectory(re(re(basename(url))))

  grid_file <- download_gz_file_and_unzip(url, download_dir)

  # Provide a copy of the projection file in the download folder
  provide_projection_file(grid_file)

  #dir(download_dir)
  raster::raster(grid_file)
}

# provide_projection_file ------------------------------------------------------
provide_projection_file <- function(file)
{
  download.file(url_projection(), method = "auto", destfile = file.path(
    dirname(file),
    replace_file_extension(basename(file), ".prj")
  ))
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
