# list_url ---------------------------------------------------------------------

#' List Files or Folders in a FTP Directory
#'
#' @param url URL to FTP server, including "ftp://"
#' @param recursive logical indicating whether to list files in all
#'   subdirectories (default: \code{FALSE})
#' @param max_depth maximum folder depth to consider when
#'   \code{recursive = TRUE}
#' @param \dots arguments passed to \code{kwb.dwd:::try_to_get_url}, such as
#'   \code{n_trials}, \code{timeout}, or \code{sleep_time}
#' @param depth for start depth when \code{recursive = TRUE}
#' @param full_info if \code{TRUE}, not only the path and filename are returned
#'   but also the file properties. The default is \code{FALSE}.
#' @param curl RCurl handle passed to \code{kwb.dwd:::try_to_get_url}
#' @export
#'
list_url <- function(
  url, recursive = ! is.na(max_depth), max_depth = NA, ..., depth = 0,
  full_info = FALSE, curl = RCurl::getCurlHandle(ftp.use.epsv = TRUE)
)
{
  #kwb.utils::assignPackageObjects("kwb.dwd")
  stopifnot(is.character(url))
  stopifnot(length(url) == 1)

  # Append slash if necessary
  url <- kwb.utils::assertFinalSlash(url)

  # Get a response from the FTP server
  response <- try_to_get_url(url, curl = curl, ...)
  #response <- kwb.dwd:::try_to_get_url(url)

  # Return empty character vector if the response is NULL or equal to an empty
  # string. A response of NULL indicates that an error occurred when reading
  # from the url. In this case, the attribute "failed" is set to the URL that
  # failed to be accessed.
  if (is.null(response) || grepl("^\\s*$", response)) {

    return(structure(
      if (full_info) data.frame() else character(),
      failed = if (is.null(response)) url
    ))
  }

  # Convert response string to data frame
  info <- response_to_data_frame(response)

  # if (full_info) {
  #   return(info_to_file_info(info))
  # }

  # Extract permission strings (to check for the directory flag "d")
  permissions <- kwb.utils::selectColumns(info, "permissions")

  # Extract the file names
  files <- kwb.utils::selectColumns(info, "file")

  # Which files represent directories?
  is_directory <- grepl("^d", permissions)

  # Are we at maximum depth?
  at_maximum_depth <- (! is.na(max_depth) && depth == max_depth)

  # Return the file list if no recursive listing is requested or if we are
  # already at maximum depth or if there are no directories
  if (! recursive || at_maximum_depth || ! any(is_directory)) {

    # Indicate directories with trailing slash
    info$file <- indicate_directories(files, is_directory)

    return (if (full_info) {
      info_to_file_info(info)
    } else {
      info$file
    })
  }

  # If we arrive here, a recursive listing is requested

  # If there are directories, list them by calling this function recursively
  files_in_dirs <- if (any(is_directory)) {

    # URLs representing directories
    directories <- files[is_directory]

    # List all directories
    n_directories <- length(directories)
    url_lists <- lapply(seq_len(n_directories), function(i) {
      cat(sprintf("%s%d/%d: ", repeated("  ", depth), i, n_directories))
      list_url(
        paste0(url, directories[i]),
        recursive = recursive,
        ...,
        depth = depth + 1,
        max_depth = max_depth,
        full_info = full_info,
        curl = curl
      )
    })

    merge_url_lists(url_lists, directories, full_info)

  } # else NULL implicitly

  # Merge files at this level with files in subdirectories
  all_files <- if (full_info) rbind(

    info_to_file_info(info[! is_directory, ]),
    files_in_dirs

  ) else c(

    files[! is_directory], # files at this level
    files_in_dirs # files in subdirectories
  )

  # Return the sorted file list with attribute "failed" if any directory URL
  # could not be accessed
  result <- if (full_info) {
    kwb.utils::resetRowNames(all_files[order(all_files$file), ])
  } else {
    sort(all_files)
  }

  structure(result, failed = attr(files_in_dirs, "failed"))
}

# response_to_data_frame -------------------------------------------------------
response_to_data_frame <- function(response)
{
  # Split response at new line character into rows
  rows <- strsplit(response, "\r?\n")[[1]]

  # Widths of the info parts of the rows in number of characters
  pattern <- "^((([^ ]+) +){8})"
  info_list <- kwb.utils::subExpressionMatches(pattern, rows, select = 1)
  info_widths <- nchar(unlist(lapply(info_list, "[[", 1)))

  # Read the info block into a data frame
  text <- unlist(lapply(seq_along(rows), function(i) {
    substr(rows[i], 1, info_widths[i])
  }))

  info <- utils::read.table(text = text, stringsAsFactors = FALSE)

  # Name the columns
  names(info) <- c(
    "permissions", "links", "user", "group", "size", "month", "day", "time"
  )

  # Append the file names (keeping possible spaces!)
  info$file <- unlist(lapply(seq_along(rows), function(i) {
    row <- rows[i]
    substr(row, info_widths[i] + 1, nchar(row))
  }))

  # Return info data frame
  info
}

# info_to_file_info ------------------------------------------------------------
info_to_file_info <- function(info, url = NULL)
{
  months <- list(
    Jan = 1L, Feb = 2L, Mar = 3L, Apr = 04L, May = 05L, Jun = 06L,
    Jul = 7L, Aug = 8L, Sep = 9L, Oct = 10L, Nov = 11L, Dec = 12L
  )

  info$type <- ifelse(grepl("^d", info$permissions), "directory", "file")

  is_this_year <- grepl(":", info$time)

  info$year <- info$time
  info$year[is_this_year] <- format(Sys.Date(), "%Y") # current year

  info$time[! is_this_year] <- "00:00"

  info$modification_time <- sprintf(
    "%04d-%02d-%02d %s",
    as.integer(info$year),
    sapply(info$month, kwb.utils::selectElements, x = months),
    as.integer(info$day),
    info$time
  )

  info <- kwb.utils::removeColumns(info, c("year", "month", "day", "time"))

  kwb.utils::moveColumnsToFront(info, c(
    "file", "type", "size", "permissions", "modification_time", "user", "group"
  ))
}

# merge_url_lists --------------------------------------------------------------
merge_url_lists <- function(url_lists, directories, full_info)
{
  #url_lists <- list(character(), character())
  stopifnot(is.list(url_lists))

  if (length(url_lists) == 0) {
    return(if (full_info) data.frame() else character())
  }

  # Merge the file lists returned for each directory
  files <- kwb.utils::excludeNULL(lapply(seq_along(url_lists), function(i) {
    #i <- 1
    urls <- url_lists[[i]]
    if (full_info && nrow(urls)) {
      urls$file <- paste0(directories[i], "/", urls$file)
      urls
    } else if (length(urls)) {
      paste0(directories[i], "/", urls)
    }
  }))

  if (length(files) == 0) {
    return(if (full_info) data.frame() else character())
  }

  # Merge the URLs of directories that could not be read
  failed <- kwb.utils::excludeNULL(lapply(url_lists, attr, which = "failed"))

  # Return the vector of files with an attribute "failed"
  result <- if (full_info) {
    do.call(rbind, files)
  } else {
    unlist(files)
  }

  structure(result, failed = unlist(failed))
}
