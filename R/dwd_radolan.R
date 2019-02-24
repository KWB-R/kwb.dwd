#' Get URLs to Available Radolan Files on DWD Server
#'
#' @param start_daily month string (yyyy-mm) of first daily record.
#'   Default: "2006-10"
#' @param start_hourly month string (yyyy-mm) of first hourly record.
#'   Default: "2005-06"
#' @param end_daily month string (yyyy-mm) of last daily record. Defaults to the
#'   current month.
#' @param end_hourly month string (yyyy-mm) of last hourly records. Defaults to
#'   \code{end_daily}.
#' @importFrom magrittr %>%
#' @importFrom kwb.utils resolve
#' @importFrom lubridate rollback
#' @importFrom stringr str_replace
#' @importFrom fs dir_create
#' @return list with "daily_historical_urls" and "hourly_historical_urls"
#' @export
get_radolan_urls <- function(
  start_daily = "2006-10",
  start_hourly = "2005-06",
  end_daily = format(format = "%Y-%m", lubridate::rollback(
    Sys.Date(), roll_to_first = TRUE
  )),
  end_hourly = end_daily
)
{
  grammar <- list(
    base = "ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany",
    endpoint = "<base>/<resolution>/radolan/<currentness>/<subdir>",
    subdir_daily = "",
    subdir_hourly = "bin/"
  )

  get_base_url <- function(resolution, currentness) {
    kwb.utils::resolve(
      "endpoint", grammar,
      resolution = resolution,
      currentness = currentness,
      subdir = paste0("subdir_", resolution)
    )
  }

  # Define helper function
  url_file <- function(base, dates, prefixes, change_year) {
    years <- lubridate::year(dates)
    months <- lubridate::month(dates)
    # Trick: FALSE = 0 -> use first prefix, TRUE = 1 -> use second prefix
    prefixes <- prefixes[(years >= change_year) + 1]
    sprintf("%s%s/%s%02d%02d.tar.gz", base, years, prefixes, years, months)
  }

  # Return download links for "hourly" and "daily" in a list
  list(
    daily_historical_urls = url_file(
      base = get_base_url("daily", "historical"),
      dates = month_sequence(start_daily, end_daily),
      prefixes = c("SF-", "SF"),
      change_year = 2009
    ),
    hourly_historical_urls = url_file(
      base = get_base_url("hourly", "historical"),
      dates = month_sequence(start_hourly, end_hourly),
      prefixes = c("RW-", "RW"),
      change_year = 2006
    )
  )
}

#' Download Radolan Files on DWD Server
#' @param resolution temporal resolution, one of "daily" or "hourly" (default:
#'   "daily")
#' @param export_dir export directory (default: "data" in current working
#'   directory)
#' @param \dots arguments passed to \code{get_radolan_urls}
#' @importFrom magrittr %>%
#' @importFrom kwb.utils catAndRun
#' @importFrom fs dir_create
#' @importFrom utils download.file
#' @return list with "daily_historical_urls" and "hourly_historical_urls"
#' @export
#' @examples
#' \dontrun{download_radolan(resolution = "daily")}
download_radolan <- function(resolution = "daily", export_dir = "data", ...) {

  if (! resolution %in% c("daily", "hourly")) {

    stop("resolution must be one of 'daily', 'hourly'", call. = FALSE)
  }

  # Define helper function
  download_historical <- function(url, resolution) {

    hist_dir <- sprintf("%s/%s/historical", resolution, export_dir)

    fs::dir_create(hist_dir, recursive = TRUE)

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

  kwb.utils::catAndRun(
    messageText = sprintf("Download: '%s' historical radolan data", resolution),
    newLine = 3,
    expr = {
      sapply(
        X = get_radolan_urls(...)[[paste0(resolution, "_historical_urls")]],
        FUN = download_historical,
        resolution = resolution
      )
    }
  )
}
