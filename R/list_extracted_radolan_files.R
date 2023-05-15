# list_extracted_radolan_files -------------------------------------------------

#' List the Locally Available Extracted Files
#'
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
