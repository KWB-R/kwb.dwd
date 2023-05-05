# provide_projection_file ------------------------------------------------------
#' @importFrom kwb.utils catAndRun catIf replaceFileExtension stopFormatted
provide_projection_file <- function(file, dbg = FALSE)
{
  target_file <- kwb.utils::replaceFileExtension(file, ".prj")

  if (file.exists(target_file)) {

    kwb.utils::catIf(dbg, sprintf(
      "There is already a projection file: %s\n", target_file
    ))

    return()
  }

  copy_file(
    from = default_projection_file(),
    to = target_file
  )
}

# default_projection_file ------------------------------------------------------
default_projection_file <- function(quiet = TRUE)
{
  url <- "https://opendata.dwd.de/climate_environment/CDC/help/gk3.prj"

  download_if_not_there(url = url, quiet = quiet, file = file.path(
    system.file("extdata", package = "kwb.dwd"),
    basename(url)
  ))
}
