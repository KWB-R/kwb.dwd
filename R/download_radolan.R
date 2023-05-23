#' Download Radolan Files on DWD Server
#' @param resolution temporal resolution, one of "daily" or "hourly" (default:
#'   "daily")
#' @param export_dir export directory (default: "data" in current working
#'   directory)
#' @param \dots arguments passed to [get_radolan_urls], such as
#'   \code{start_daily}, \code{start_hourly}, \code{end_daily},
#'   \code{end_hourly}
#' @return list with "daily_historical_urls" and "hourly_historical_urls"
#' @importFrom fs dir_create
#' @importFrom kwb.utils catAndRun
#' @importFrom magrittr %>%
#' @importFrom utils download.file
#' @export
#' @examples
#' \dontrun{download_radolan(resolution = "daily")}
download_radolan <- function(resolution = "daily", export_dir = "data", ...)
{
  if (! resolution %in% c("daily", "hourly")) {
    clean_stop("resolution must be one of 'daily', 'hourly'")
  }

  # Define helper function
  download_historical <- function(url, resolution) {

    hist_dir <- sprintf("%s/%s/historical", export_dir, resolution)

    fs::dir_create(hist_dir, recurse = TRUE)

    export_path <- sprintf("%s/%s", hist_dir, basename(url))

    kwb.utils::catAndRun(
      messageText = sprintf(
        'Download: "%s, historical" and save to %s', resolution, export_path
      ),
      newLine = 3,
      expr = try(
        utils::download.file(url = url, destfile = export_path, mode = "wb")
      )
    )
  }

  arguments <- list(...)
  #arguments <- list()

  urls <- do.call(get_radolan_urls, arguments)

  urls_historical <- urls[[paste0(resolution, "_historical_urls")]]

  kwb.utils::catAndRun(
    messageText = sprintf("Download: '%s' historical radolan data", resolution),
    newLine = 3,
    expr = sapply(urls_historical, download_historical, resolution = resolution)
  )
}
