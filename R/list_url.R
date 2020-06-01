# list_url ---------------------------------------------------------------------

#' List Files or Folders in a FTP Directory
#'
#' @param url URL to FTP server, including "ftp://"
#' @param recursive logical indicating whether to list files in all
#'   subdirectories (default: \code{FALSE})
#' @param max_depth maximum folder depth to consider when
#'   \code{recursive = TRUE}
#' @param full_info if \code{TRUE}, not only the path and filename are returned
#'   but also the file properties. The default is \code{FALSE}.
#' @param \dots arguments passed to \code{kwb.dwd:::try_to_get_url}, such as
#'   \code{n_trials}, \code{timeout}, or \code{sleep_time}
#' @export
#'
list_url <- function(
  url = ftp_path_cdc(),
  recursive = ! is.na(max_depth),
  max_depth = NA,
  full_info = FALSE,
  ...
)
{
  result <- listToDepth(
    path = url,
    recursive = recursive,
    max_depth = max_depth,
    full_info = full_info,
    ...,
    curl = RCurl::getCurlHandle(ftp.use.epsv = TRUE),
    depth = 0,
    FUN = list_contents
  )

  if (full_info) {

    result

  } else {

    # Accessor function
    pull <- function(x) kwb.utils::selectColumns(result, x)

    # Indicate directories with trailing slash
    structure(
      indicate_directories(pull("file"), pull("isdir")),
      failed = attr(result, "failed")
    )
  }
}
