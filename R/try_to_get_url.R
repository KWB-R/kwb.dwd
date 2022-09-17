# try_to_get_url ---------------------------------------------------------------
#' try to get url
#'
#' @param url url
#' @param n_trials n_trails (default: 3)
#' @param timeout timeout (default: NULL)
#' @param sleep_time sleep time (default: 5)
#' @param user_pwd user password (default: NULL)
#' @param ... additional arguments passed to \code{RCurl::getURL}
#' @param dbg debug (default: TRUE)
#'
#' @return ???
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom RCurl curlOptions getCurlOptionsConstants
#' @importFrom kwb.utils catIf
try_to_get_url <- function(
  url, n_trials = 3, timeout = NULL, sleep_time = 5, user_pwd = NULL, ...,
  dbg = TRUE
)
{
  url <- assert_url(url)

  success <- FALSE
  trial <- 0

  if (is.null(timeout)) {
    timeout <- RCurl::getCurlOptionsConstants()[["connecttimeout"]]
  }

  curl_options <- RCurl::curlOptions(connecttimeout = timeout)

  if (! is.null(user_pwd)) {
    curl_options <- c(curl_options, RCurl::curlOptions(userpwd = user_pwd))
  }

  kwb.utils::catIf(dbg, sprintf("%s:", url))

  while (! success && trial < n_trials) {

    trial <- trial + 1
    response <- try(silent = TRUE, RCurl::getURL(url, .opts = curl_options, ...))
    success <- ! inherits(response, "try-error")

    if (! success && trial == 1) {
      cat(" ")
      cat_progress(0, n_trials)
      cat_progress(1, n_trials, success)
    }

    if (trial > 1) {
      cat_progress(trial, n_trials, success)
    }

    if (! success && trial < n_trials) {
      Sys.sleep(sleep_time)
    }
  }

  kwb.utils::catIf(dbg, ifelse(success, " ok.\n", " failed.\n"))

  if (success) {
    response
  } # else NULL implicitly
}
