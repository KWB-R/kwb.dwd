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

  prj_file <- default_projection_file()

  if (! file.exists(prj_file)) {
    url <- url_projection()
    kwb.utils::catAndRun(
      sprintf(
        "Downloading projection file from\n  %s\nto\n  %s",
        url, prj_file
      ),
      newLine = 3L,
      download.file(url, prj_file, method = "auto")
    )
  }

  success <- file.copy(from = prj_file, to = destfile)

  if (! all(success)) {
    stop(sprintf("Could not copy %s to %s", prj_file, destfile))
  }
}

# url_projection ---------------------------------------------------------------
url_projection <- function()
{
  "https://opendata.dwd.de/climate_environment/CDC/help/gk3.prj"
}

# default_projection_file ------------------------------------------------------
default_projection_file <- function()
{
  file.path(system.file("extdata", package = "kwb.dwd"), "gk3.prj")
}
