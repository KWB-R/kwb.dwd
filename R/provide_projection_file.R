# provide_projection_file ------------------------------------------------------
provide_projection_file <- function(file, dbg = FALSE)
{
  destfile <- file.path(
    dirname(file),
    kwb.utils::replaceFileExtension(basename(file), ".prj")
  )

  if (file.exists(destfile)) {
    kwb.utils::catIf(
      dbg, "There is already a projection file: ", destfile, "\n"
    )
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
