# read_asc_gz_file -------------------------------------------------------------

#' Read Zipped ESRI-Ascii-Grid File (from URL)
#'
#' @param file path to zipped file in ESRI-ascii-grid format (.asc.gz)
#' @param url optional. URL to zipped file in ESRI-ascii-grid format (.asc.gz)
#' @return object of class "RasterLayer"
#' @export
#' @importFrom kwb.utils callWith removeExtension tempSubdirectory
#' @importFrom raster raster
#' @seealso
#'  * [read_binary_radolan_file],
#'  * [unzip_asc_gz_file].
read_asc_gz_file <- function(file, url = NULL)
{
  target_dir <- if (is.null(url)) {
    dirname(file)
  } else {
    temp_dir(template = kwb.utils::removeExtension(url))
  }

  # Call the unzip function setting either "url" or "file" argument
  grid_file <- unzip_asc_gz_file(
    file = if (is.null(url)) file else url,
    target_dir = target_dir
  )

  #dir(target_dir)

  # Read the .asc file into a raster object (with appropriate projection)
  read_asc_file(grid_file)
}

# read_asc_file ----------------------------------------------------------------

#' Read Raster Data from .ASC File
#'
#' @param file path to .asc file
#' @param projection projection string used in Radolan data. Currently not used!
#' @param dbg logical indicating whether to show debug messages
#' @importFrom kwb.utils catAndRun
#' @importFrom raster `crs<-` raster
read_asc_file <- function(
    file,
    projection = get_radolan_projection_string(),
    dbg = TRUE
)
{
  kwb.utils::catAndRun(
    sprintf("Reading %s using raster::raster()", basename(file)),
    dbg = dbg,
    expr = {

      # Provide a copy of the projection file in the same folder
      provide_projection_file(file)
      result <- raster::raster(file)

      #result <- raster::raster(file, values = TRUE)
      #raster::crs(result) <- projection

      result
    }
  )
}
