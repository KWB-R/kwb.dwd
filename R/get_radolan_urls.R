#' Get URLs to Available Radolan Files
#'
#' Get URLs to available radolan files on the DWD FTP server below this base
#' address: \url{ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany}. The user can
#' choose between daily records and hourly records that are located at different
#' paths on the server. The paths to the files are not read from the FTP server
#' but generated, given the knowledge of where the files should reside and how
#' they are expected to be named.
#'
#' @param start_daily month string (yyyy-mm) of first daily record.
#'   Default: "2006-10"
#' @param start_hourly month string (yyyy-mm) of first hourly record.
#'   Default: "2005-06"
#' @param end_daily month string (yyyy-mm) of last daily record. Defaults to the
#'   current month.
#' @param end_hourly month string (yyyy-mm) of last hourly records. Defaults to
#'   \code{end_daily}.
#'
#' @importFrom magrittr %>%
#' @importFrom kwb.utils resolve
#' @importFrom lubridate rollback
#' @importFrom stringr str_replace
#' @importFrom fs dir_create
#'
#' @return list with "daily_historical_urls" and "hourly_historical_urls"
#'
#' @export
#'
#' @examples
#' # Get all expected URLs
#' urls <- kwb.dwd::get_radolan_urls()
#'
#' # Show the first URLs of each category (daily, hourly)
#' head(urls$daily_historical_urls)
#' head(urls$hourly_historical_urls)
#'
#' # Set the start and end months
#' urls <- kwb.dwd::get_radolan_urls(
#'   start_daily = "2017-01", end_daily = "2017-03"
#' )
#'
#' urls$daily_historical_urls
#'
get_radolan_urls <- function(
  start_daily = "2006-10",
  start_hourly = "2005-06",
  end_daily = format(
    format = "%Y-%m", lubridate::rollback(Sys.Date(), roll_to_first = TRUE)
  ),
  end_hourly = end_daily
)
{
  # helper function
  get_year_month <- function(start, end) {
    yyyymm <- function(yyyy_mm) gsub("-", "", yyyy_mm)
    year_month_01 <- as.character(month_sequence(yyyymm(start), yyyymm(end)))
    substr(gsub("-", "", year_month_01), 1, 6)
  }

  yyyymm_daily <- get_year_month(start_daily, end_daily)
  yyyymm_hourly <- get_year_month(start_hourly, end_hourly)

  # Return download links for "hourly" and "daily" in a list
  list(
    daily_historical_urls = get_radolan_url("daily", yyyymm_daily),
    hourly_historical_urls = get_radolan_url("hourly", yyyymm_hourly)
  )
}

# get_radolan_url --------------------------------------------------------------
get_radolan_url <- function(frequency, year_month)
{
  # Define first available year and month and year when naming schemes changed
  starts <- c(hourly = "200506", daily = "200610")
  switches <- c(hourly = 2006, daily = 2009)

  # Check argument "frequency"
  frequency <- safe_element(frequency, names(starts))

  # Check argument "year_month"
  if (! all(grepl("^20[0-9]{2}[01][0-9]$", year_month))) clean_stop(
    "year_month must be a string of six numeric characters giving the ",
    "year and month of the data to be downloaded: e.g. '200807' for July 2008."
  )

  # Check that year_month strings are not before the first available string
  if (any(year_month < starts[frequency])) clean_stop(sprintf(
    "The first avaible year and month is '%s'. You requested: '%s'",
    starts[[frequency]], kwb.utils::stringList(year_month)
  ))

  # Extract the year number
  year <- as.integer(substr(year_month, 1, 4))

  # Use old or new version of file name?
  is_old <- year < switches[frequency]

  # Set subdirectory and filename prefix depending on frequency and is_old
  if (frequency == "hourly") {

    subdir <- "bin/"
    prefix <- ifelse(is_old, "RW-", "RW")

  } else if (frequency == "daily") {

    subdir <- ""
    prefix <- ifelse(is_old, "SF-", "SF")
  }

  # Root path of the URL
  ftp_root <- "ftp://ftp-cdc.dwd.de/pub/CDC"

  # Define the URL path's format string for sprintf()
  format_string <- "%s/grids_germany/%s/radolan/historical/%s%s"

  # Compose the URL's path
  path <- sprintf(format_string, ftp_root, frequency, subdir, year)

  # Compose the full URL
  sprintf("%s/%s%s.tar.gz", path, prefix, year_month)
}
