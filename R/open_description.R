# open_description -------------------------------------------------------------

#' Open Description for DWD Record in Web Browser
#'
#' This function looks for text or PDF files in the same folder as a DWD data
#' file, asks the user which file to open if there is more than one file and
#' tries to open the file in the web browser.
#'
#' @param url URL to DWD data file on ftp server
#' @export
open_description <- function(url)
{
  files <- find_description_files(url, full_names = TRUE)

  n_files <- length(files)

  if (n_files == 0L) {
    cat("No description files found.")
    return()
  }

  if (n_files > 1L) {

    kwb.utils::clearConsole()

    file <- select.list(
      basename(files),
      title = "Which file should I open? (0 = Cancel)"
    )

    if (file == "") {
      return()
    }
  }

  cat("Trying to open", file, "in the web browser ...")
  browseURL(gsub("^ftp:", "http:", files[file == basename(files)]))
}

# find_description_files -------------------------------------------------------

#' For a Given URL, Try to Find Description Files
#'
#' @param url URL to a data file at ftp server by Deutscher Wetterdienst
#' @param full_names logical. If \code{TRUE} the full URLs are returned,
#'   otherwise only the file names. The default is \code{FALSE}.
#' @return vector of character with file names of or full URLs to all files with
#'   extensions ".txt" or ".pdf" that are in the same folder as the file that
#'   \code{url} points to
find_description_files <- function(url, full_names = FALSE)
{
  urls <- kwb.dwd::list_url(dirname(url), full_names = full_names)
  grep("\\.(pdf|txt)$", urls, ignore.case = TRUE, value = TRUE)
}
