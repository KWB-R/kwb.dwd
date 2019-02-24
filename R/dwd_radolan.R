# get_url_dictionary -----------------------------------------------------------
get_url_dictionary <- function()
{
  list(
    base_url = "ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany",
    radolan = "<base_url>/<resolution>/radolan",
    #daily_radolan = "<base_url>/daily/radolan",
    #hourly_radolan = "<base_url>/hourly/radolan",
    daily_historical = "<daily_radolan>/historical",
    daily_recent = "<daily_radolan>/recent",
    hourly_historical = "<hourly_radolan>/historical/bin",
    hourly_recent = "<hourly_radolan>/recent/bin"
  )
}

#' Get URLs to Available Radolan Files on DWD Server
#'
#' @param date_start_daily date string (yyyy-mm-dd) of first day of daily
#'   records. Default: "2006-10-01"
#' @param date_start_hourly date string (yyyy-mm-dd) of first day of hourly
#'   records. Default: "2005-06-01"
#' @param date_end_daily date string (yyyy-mm-dd) of last day of daily
#'   records. Defaults to the first day of the current month.
#' @param date_end_hourly date string (yyyy-mm-dd) of last day of hourly
#'   records. Defaults to \code{date_end_daily}.
#' @importFrom magrittr %>%
#' @importFrom kwb.utils resolve
#' @importFrom lubridate rollback
#' @importFrom stringr str_replace
#' @importFrom fs dir_create
#' @return list with "daily_historical_urls" and "hourly_historical_urls"
#' @export
get_radolan_urls <- function(
  date_start_daily = "2006-10-01", date_start_hourly = "2005-06-01",
  date_end_daily = lubridate::rollback(Sys.Date(), roll_to_first = TRUE),
  date_end_hourly = date_end_daily
) {

  base_urls <- kwb.utils::resolve(get_url_dictionary())

  # Define helper function
  date_sequence <- function(date_start, date_end) {
    seq(lubridate::ymd(date_start), lubridate::ymd(date_end), by = 'months')
  }

  url_hist_file <- function(url_start, dates, file_format) {
    paste0(url_start, format(dates, format = file_format), sep = "/") %>%
      stringr::str_replace("/(SF|RW)-", "/\\1")
  }

  # Return download links for "hourly" and "daily" in a list
  list(
    daily_historical_urls = url_hist_file(
      url_start = base_urls$daily_historical,
      dates = date_sequence(date_start_daily, date_end_daily),
      file_format = "%Y/SF-%Y%m.tar.gz"
    ),
    hourly_historical_urls = url_hist_file(
      url_start = base_urls$hourly_historical,
      dates = date_sequence(date_start_hourly, date_end_hourly),
      file_format = "%Y/RW-%Y%m.tar.gz"
    )
  )
}

#' Download Radolan Files on DWD Server
#' @param resolution temporal resolution, one of "daily" or "hourly" (default:
#'   "daily")
#' @param export_dir export directory (default: "data" in current working
#' directory)
#' @importFrom magrittr %>%
#' @importFrom kwb.utils catAndRun
#' @importFrom fs dir_create
#' @importFrom utils download.file
#' @return list with "daily_historical_urls" and "hourly_historical_urls"
#' @export
#' @examples
#' \dontrun{download_radolan(resolution = "daily")}
download_radolan <- function(resolution = "daily", export_dir = "data") {

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
        X = get_radolan_urls()[[paste0(resolution, "_historical_urls")]],
        FUN = download_historical,
        resolution = resolution
      )
    }
  )
}
