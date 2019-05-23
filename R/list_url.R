# list_url ---------------------------------------------------------------------

#' List Files or Folders in a FTP Directory
#'
#' @param url URL to FTP server, including "ftp://"
#' @param recursive logical indicating whether to list files in all
#'   subdirectories (default: \code{FALSE})
#' @param \dots arguments passed to \code{kwb.dwd:::try_to_get_url}, such as
#'   \code{n_trials}, \code{timeout}, or \code{sleep_time}
#' @export
#'
list_url <- function(url, recursive = FALSE, ...)
{
  stopifnot(is.character(url))
  stopifnot(length(url) == 1)

  # Append slash if necessary
  if (! grepl("/$", url)) {
    url <- paste0(url, "/")
  }

  # Get a response from the FTP server
  response <- try_to_get_url(url, ...)
  #response <- kwb.dwd:::try_to_get_url(url)

  # Return NULL if the response is NULL (in case of an error) or if the
  # response is empty
  if (is.null(response) || grepl("^\\s*$", response)) {
    return(structure(character(), failed = url))
  }

  info <- response_to_data_frame(response)

  permissions <- kwb.utils::selectColumns(info, "permissions")
  files <- kwb.utils::selectColumns(info, "file")

  is_directory <- grepl("^d", permissions)

  if (! recursive) {
    return(files)
  }

  if (any(is_directory)) {

    # URLs representing directories
    directories <- files[is_directory]

    # List all directories
    result <- lapply(
      paste0(url, directories),
      list_url,
      recursive = recursive,
      ...
    )

    files_in_dirs <- unlist(lapply(seq_along(directories), function(i) {
      files <- result[[i]]
      if (length(files)) {
        paste0(directories[i], "/", files)
      }
    }))

    failed <- unlist(lapply(result, attr, which = "failed"))

  } else {

    files_in_dirs <- NULL
    failed <- NULL
  }

  structure(sort(c(files[! is_directory], files_in_dirs)), failed = failed)
}

# try_to_get_url ---------------------------------------------------------------
try_to_get_url <- function(url, n_trials = 3, timeout = NULL, sleep_time = 5)
{
  stopifnot(is.character(url))
  stopifnot(length(url) == 1)

  success <- FALSE
  trial <- 0

  if (is.null(timeout)) {
    timeout <- RCurl::getCurlOptionsConstants()[["connecttimeout"]]
  }

  curl_options <- RCurl::curlOptions(connecttimeout = timeout)

  cat(sprintf("%s:", url))

  while (! success && trial < n_trials) {

    trial <- trial + 1
    response <- try(silent = TRUE, RCurl::getURL(url, .opts = curl_options))
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

  cat0(ifelse(success, " ok.\n", " failed.\n"))

  if (! success) {
    return(NULL)
  }

  response
}

# response_to_data_frame -------------------------------------------------------
response_to_data_frame <- function(response)
{
  # Split response at new line character into rows
  rows <- strsplit(response, "\r?\n")[[1]]

  # Widths of the info parts of the rows in number of characters
  pattern <- "^(((\\S+)\\s+){8})"
  info_list <- kwb.utils::subExpressionMatches(pattern, rows, select = 1)
  info_widths <- nchar(unlist(lapply(info_list, "[[", 1)))

  # Read the info block into a data frame
  #text <- unlist(lapply(rows, substr, 1, info_width))
  text <- unlist(lapply(seq_along(rows), function(i) substr(rows[i], 1, info_widths[i])))
  info <- utils::read.table(text = text, stringsAsFactors = FALSE)

  # Name the columns
  names(info) <- c(
    "permissions", "V2", "V3", "group", "size", "month", "day", "time"
  )

  # Append the file names (keeping possible spaces!)
  info$file <- unlist(lapply(seq_along(rows), function(i) {
    row <- rows[i]
    substr(row, info_widths[i] + 1, nchar(row))
  }))

  # Return info data frame
  info
}
