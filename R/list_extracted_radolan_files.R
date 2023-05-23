# list_extracted_radolan_files -------------------------------------------------

#' List the Locally Available Extracted Files
#'
#' @param from begin of time interval as "yyyymm" string (4 digits year + 2
#'   digits month)
#' @param to end of time interval as "yyyymm" string (4 digits year + 2 digits
#'   month)
#' @param resolution temporal resolution, currently one of "daily", "hourly"
#' @param format file format, currently one of "asc" (ASCII text), "bin"
#'   (binary)
#' @return paths to files on the local drive, below the folder returned by
#'   [temp_dir], that contain data in the requested \code{format},
#'   time \code{resolution} and time interval (specified by \code{from} and
#'   \code{to}).
#' @export
list_extracted_radolan_files <- function(from, to, resolution, format)
{
  # resolution = "daily"
  # format = "bin"
  # from = "201601"
  # to = "201612"

  safe_element(resolution, c("daily", "hourly"))
  safe_element(format, c("asc", "bin"))

  pattern <- kwb.utils::selectElements(elements = format, list(
    asc = "\\.asc$",
    bin = "--bin(\\.gz)?$" # files may optionally be zipped
  ))

  files <- "grids_germany/%s/radolan/historical/%s" %>%
    sprintf(resolution, format) %>%
    temp_dir() %>%
    dir(pattern = pattern, recursive = TRUE, full.names = TRUE) %>%
    filter_by_month_range(from, to)

  if (length(files) == 0L) {
    message(paste(collapse = "\n", c(
      "No matching files found on the local drive.",
      "Please run download_and_extract_radolan() first."
    )))
  }

  files
}
