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
#' @param full_names if \code{TRUE}, the full URLs are returned, otherwise (the
#'   default) only the paths relative to \code{url}.
#' @param \dots arguments passed to \code{kwb.dwd:::try_to_get_url}, such as
#'   \code{n_trials}, \code{timeout}, or \code{sleep_time}
#' @export
#' @importFrom kwb.utils listToDepth selectColumns
#' @importFrom RCurl getCurlHandle
list_url <- function(
  url = ftp_path_cdc(),
  recursive = ! is.na(max_depth),
  max_depth = NA,
  full_info = FALSE,
  full_names = FALSE,
  ...
)
{
  result <- kwb.utils::listToDepth(
    path = url,
    max_depth = ifelse(isTRUE(recursive), max_depth, 0L),
    full_info = full_info,
    ...,
    curl = RCurl::getCurlHandle(ftp.use.epsv = TRUE),
    depth = 0,
    FUN = list_ftp_contents
  )

  if (full_info) {
    return(result)
  }

  paths <- kwb.utils::selectColumns(result, "file")

  if (full_names) {
    paths <- file.path(url, paths)
  }

  # Indicate directories with trailing slash
  structure(failed = attr(result, "failed"), indicate_directories(
    paths, kwb.utils::selectColumns(result, "isdir")
  ))
}
