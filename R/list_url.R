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
  url = ftp_path_cdc(),
  recursive = ! is.na(max_depth),
  max_depth = NA,
  ...,
  depth = 0,
  full_info = FALSE,
  curl = RCurl::getCurlHandle(ftp.use.epsv = TRUE)
)
{
  #kwb.utils::assignPackageObjects("kwb.dwd")

  # Check URL and append slash if necessary
  url <- assert_url(url)

  # List the files in the folder specified by URL
  info <- get_file_info_from_url(url, curl, full_info, ...)

  # Return if there is nothing to see
  if (is_empty(info)) {
    return(info)
  }

  # Extract the file names
  files <- get_file(info)

  # Which files represent directories?
  is_directory <- row_represents_directory(info)

  # Return the file list if no recursive listing is requested or if we are
  # already at maximum depth or if there are no directories
  if (need_to_return(recursive, depth, max_depth, is_directory)) {

    # Indicate directories with trailing slash
    info <- set_file(info, indicate_directories(files, is_directory))

    return(finish_file_info(info, full_info))
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

  # Merge files at this level with files in subdirectories. Return the sorted
  # file list with attribute "failed" if any directory URL could not be accessed
  structure(failed = attr(files_in_dirs, "failed"), sort_or_order(
    x = merge_file_info(info, files, files_in_dirs, is_directory),
    by = "file"
  ))
}

# get_file ---------------------------------------------------------------------
get_file <- function(df)
{
  kwb.utils::selectColumns(df, "file")
}

# set_file ---------------------------------------------------------------------
set_file <- function(df, file)
{
  kwb.utils::setColumns(df, file = file, dbg = FALSE)
}

# get_file_info_from_url -------------------------------------------------------
get_file_info_from_url <- function(url, curl, full_info, ...)
{
  # Get a response from the FTP server
  response <- try_to_get_url(url, curl = curl, ...)
  #response <- kwb.dwd:::try_to_get_url(url)

  # Return empty character vector if the response is NULL or equal to an empty
  # string. A response of NULL indicates that an error occurred when reading
  # from the url. In this case, the attribute "failed" is set to the URL that
  # failed to be accessed.
  if (is_empty_response(response)) {

    return(structure(
      empty_file_info(full_info),
      failed = if (is.null(response)) url
    ))
  }

  # Convert response string to data frame
  response_to_data_frame(response)
}

# is_empty_response ------------------------------------------------------------
is_empty_response <- function(response)
{
  is.null(response) || grepl("^\\s*$", response)
}

# empty_file_info --------------------------------------------------------------
empty_file_info <- function(full_info)
{
  if (full_info) {

    data.frame()

  } else {

    character()
  }
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

  # Convert the text into a data frame and name the columns
  info <- stats::setNames(
    utils::read.table(text = text, stringsAsFactors = FALSE), c(
      "permissions", "links", "user", "group", "size", "month", "day", "time"
    )
  )

  # Extract the file names (keeping possible spaces!)
  files <- unlist(lapply(seq_along(rows), function(i) {
    substring(rows[i], info_widths[i] + 1)
  }))

  # Set the file names in the info data frame and return the info data frame
  set_file(info, files)
}

# row_represents_directory -----------------------------------------------------
row_represents_directory <- function(info)
{
  # Extract permission strings (to check for the directory flag "d")
  permissions <- kwb.utils::selectColumns(info, "permissions")

  # Which files represent directories?
  grepl("^d", permissions)
}

# need_to_return ---------------------------------------------------------------
need_to_return <- function(recursive, depth, max_depth, is_directory)
{
  # Are we at maximum depth?
  at_max_depth <- (! is.na(max_depth) && depth == max_depth)

  # No recursion requested or maximum depth reached or there are no directories
  ! recursive || at_max_depth || ! any(is_directory)
}

# finish_file_info -------------------------------------------------------------
finish_file_info <- function(info, full_info)
{
  if (full_info) {

    info_to_file_info(info)

  } else {

    get_file(info)
  }
}

# info_to_file_info ------------------------------------------------------------
info_to_file_info <- function(info, url = NULL)
{
  months <- month_numbers()

  info$type <- ifelse(row_represents_directory(info), "directory", "file")

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

  # Prepare an empty result set
  empty_result <- empty_file_info(full_info)

  if (length(url_lists) == 0) {
    return(empty_result)
  }

  # Merge the file lists returned for each directory
  files <- kwb.utils::excludeNULL(dbg = FALSE, lapply(
    seq_along(url_lists),
    FUN = function(i) {
      #i <- 1
      urls <- url_lists[[i]]

      add_parent <- function(x) paste0(directories[i], "/", x)

      if (full_info && nrow(urls)) {

        set_file(urls, add_parent(get_file(urls)))

      } else if (length(urls)) {

        add_parent(urls)
      }
    }
  ))

  if (length(files) == 0) {
    return(empty_result)
  }

  # Return the vector of files with an attribute "failed" holding the merged
  # URLs of directories that could not be read
  structure(
    bind_list_elements(files),
    failed = unlist(kwb.utils::excludeNULL(dbg = FALSE, lapply(
      url_lists, attr, which = "failed"
    )))
  )
}

# merge_file_info --------------------------------------------------------------
merge_file_info <- function(info, files, files_in_dirs, is_directory)
{
  if (is.data.frame(info)) {

    rbind(info_to_file_info(info[! is_directory, ]), files_in_dirs)

  } else {

    # files at this level + files in subdirectories
    c(files[! is_directory], files_in_dirs)
  }
}
