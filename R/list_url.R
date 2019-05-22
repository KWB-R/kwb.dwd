# list_url ---------------------------------------------------------------------

#' List Files or Folders in a FTP Directory
#'
#' @param url URL to FTP server, including "ftp://"
#' @param recursive logical indicating whether to list files in all
#'   subdirectories (default: \code{FALSE})
#' @param n_attempts number of attempts when trying to access an URL.
#' @export
#'
list_url <- function(url, recursive = FALSE, n_attempts = 3)
{
  stopifnot(is.character(url))
  stopifnot(length(url) == 1)

  # Check for trailing slash and append slash if necessary
  if (! grepl("/$", url)) {
    url <- paste0(url, "/")
  }

  response <- try_to_get_url(url, n_attempts = n_attempts)

  if (is.null(response)) {
    return(NULL)
  }

  content <- utils::read.table(text = response, stringsAsFactors = FALSE)

  files <- content$V9

  is_directory <- grepl("^d", content$V1)

  if (! recursive) {
    return(files)
  }

  #files[is_directory] <- paste0(files[is_directory], "/")
  files_in_dirs <- if (any(is_directory)) {

    # URLs representing directories
    directories <- files[is_directory]
    #directory <- directories[2]

    # List all directories
    result <- lapply(
      paste0(url, directories),
      list_url,
      recursive = recursive,
      n_attempts = n_attempts
    )

    unlist(lapply(seq_along(directories), function(i) {
      paste0(directories[i], "/", result[[i]])
    }))

  } # else NULL

  sort(c(files[! is_directory], files_in_dirs))
}

# try_to_get_url ---------------------------------------------------------------
try_to_get_url <- function(url, n_attempts = 3, timeout = NULL, sleep_time = 1)
{
  stopifnot(is.character(url))
  stopifnot(length(url) == 1)

  success <- FALSE
  attempt <- 1

  if (is.null(timeout)) {
    timeout <- RCurl::getCurlOptionsConstants()[["connecttimeout"]]
  }

  curl_options <- RCurl::curlOptions(connecttimeout = timeout)

  while (! success && attempt <= n_attempts) {
    response <- try(RCurl::getURL(url, .opts = curl_options), silent = TRUE)
    success <- ! inherits(response, "try-error")
    if (! success) {
      condition <- kwb.utils::getAttribute(response, "condition")
      msg <- sprintf(
        "%s: %s (%d/%d)", url, condition$message, attempt, n_attempts
      )
      writeLines(msg)
      if (attempt < n_attempts) {
        Sys.sleep(sleep_time)
        sleep_time <- sleep_time * 2
      }
    }
    attempt <- attempt + 1
  }

  if (! success) {
    message(sprintf(
      "Could not get URL '%s' in %d attempts. Returning NULL.",
      url, n_attempts
    ))
    return(NULL)
  }

  response
}
