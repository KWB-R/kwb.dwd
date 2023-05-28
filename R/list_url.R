# list_url ---------------------------------------------------------------------

#' List Files or Folders in a FTP Directory
#'
#' @param url URL to FTP server, including "ftp://"
#' @param recursive logical indicating whether to list files in all
#'   subdirectories (default: `FALSE`)
#' @param max_depth maximum folder depth to consider when `recursive = TRUE`
#' @param full_info if `TRUE`, not only the path and filename are returned but
#'   also the file properties. The default is `FALSE`.
#' @param full_names if `TRUE`, the full URLs are returned, otherwise (the
#'   default) only the paths relative to `url`.
#' @param \dots arguments passed to `kwb.dwd:::try_to_get_url`, such as
#'   `n_trials`, `timeout`, or `sleep_time`
#' @export
#' @importFrom kwb.utils listToDepth selectColumns
#' @importFrom RCurl getCurlHandle
#' @seealso
#'  * [list_extracted_radolan_files].
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
