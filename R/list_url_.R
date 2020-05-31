# list_url_ --------------------------------------------------------------------

# In contrast to list_url(), this function always returns a data frame, at least
# with columns "file", "is_directory", if full_info = FALSE.
# @param depth for start depth when \code{recursive = TRUE}
# @param curl RCurl handle passed to \code{kwb.dwd:::try_to_get_url}
list_url_ <- function(
  url, recursive = TRUE, max_depth = 1, full_info = FALSE, ..., curl, depth = 0,
  random_failures = TRUE
)
{
  if (FALSE) {
    kwb.utils::assignPackageObjects("kwb.dwd")
    kwb.utils::assignArgumentDefaults(list_url)
    kwb.utils::assignArgumentDefaults(list_url_)
    max_depth = 1;full_info=TRUE;set.seed(1)
  }

  # Check URL and append slash if necessary
  url <- assert_url(url)

  # Shall a failure be simulated?
  mutate <- random_failures && sample(c(TRUE, FALSE), 1, prob = c(0.1, 0.9))

  # Provide URL so to for (eventually randomly modified)
  url_to_go <- paste0(url, if (mutate) "blabla")

  # Get a response from the FTP server
  response <- try_to_get_url(url_to_go, curl = curl, ...)
  #response <- try_to_get_url(url_to_go)

  # Convert response string to data frame
  info <- response_to_data_frame(response, full_info)

  # Return if info is empty. A response of NULL indicates that an error occurred
  # when reading from the URL. In this case, set the attribute "failed" to the
  # URL that failed to be accessed.
  if (is_empty(info)) {
    return(structure(info, failed = if (is.null(response)) url))
  }

  # Helper function to get an info column
  info_column <- function(x) kwb.utils::selectColumns(info, x)

  # Extract the file names
  files <- info_column("file")

  # Which files represent directories?
  is_directory <- info_column("is_directory")

  # Return the file list if no recursive listing is requested or if we are
  # already at maximum depth or if there are no directories
  if (! recursive || at_max_depth(depth, max_depth) || ! any(is_directory)) {

    # Indicate directories with trailing slash
    return(kwb.utils::setColumns(
      info, file = indicate_directories(files, is_directory)
    ))
  }

  # If we arrive here, a recursive listing is requested

  # Number of directories
  n_directories <- sum(is_directory)

  # If there are directories, list them by calling this function recursively
  files_in_dirs <- if (n_directories > 0L) {

    # Helper function to show the progress
    show_progress <- function(i) cat(sprintf(
      "%s%d/%d: ", repeated("  ", depth), i, n_directories
    ))

    # URLs representing directories
    directories <- files[is_directory]

    # List all directories
    url_lists <- lapply(seq_len(n_directories), function(i) {

      show_progress(i)

      # Recursive call of this function
      list_url_(
        url = paste0(url, directories[i]),
        recursive = recursive,
        max_depth = max_depth,
        full_info = full_info,
        ...,
        curl = curl,
        depth = depth + 1
      )
    })

    merge_url_lists(url_lists, directories, full_info)

  } # else NULL implicitly

  # Merge files at this level with files in subdirectories. Return the sorted
  # file list with attribute "failed" if any directory URL could not be accessed
  structure(
    order_by(rbind(info = info[! is_directory, ], files_in_dirs), "file"),
    failed = attr(files_in_dirs, "failed")
  )
}

# response_to_data_frame -------------------------------------------------------
response_to_data_frame <- function(response, full_info = FALSE)
{
  template <- empty_file_info(full_info)

  # Response is NULL or equal to an empty string?
  if (is.null(response) || grepl("^\\s*$", response)) {
    return(template)
  }

  info <- response_to_data_frame_raw(response)

  # Required column: is_directory
  info$is_directory <- row_represents_directory(info)

  if (! full_info) {
    return(kwb.utils::selectColumns(info, c("file", "is_directory")))
  }

  #info$type <- ifelse(info$is_directory, "directory", "file")

  # Replace columns "year_or_time", "month", "day" with "modification_time".
  # Put the most important columns first.
  main_columns_first(simplify_time_info(info))
}

# simplify_time_info -----------------------------------------------------------
simplify_time_info <- function(info)
{
  # Add column "modification_time"
  info$modification_time <- columns_to_timestamp(info)

  # Remove the columns that were used to create the timestamps
  kwb.utils::removeColumns(info, c("year_or_time", "month", "day"))
}

# columns_to_timestamp ---------------------------------------------------------
columns_to_timestamp <- function(info)
{
  pull <- function(x) kwb.utils::selectColumns(info, x)

  years_or_times <- pull("year_or_time")

  is_year <- ! grepl(":", years_or_times)

  # Compose a vector of timestamps. Use the current year in case of missing
  # years, and use midnight in case of missing times
  sprintf(
    "%04d-%02d-%02d %s",
    as.integer(ifelse(is_year, years_or_times, format(Sys.Date(), "%Y"))),
    sapply(pull("month"), kwb.utils::selectElements, x = month_numbers()),
    as.integer(pull("day")),
    ifelse(is_year, "00:00", years_or_times)
  )
}

# main_columns_first -----------------------------------------------------------
main_columns_first <- function(df)
{
  columns <- intersect(names(empty_file_info(full_info = TRUE)), names(df))

  kwb.utils::moveColumnsToFront(df, columns)
}

# response_to_data_frame_raw ---------------------------------------------------
response_to_data_frame_raw <- function(response)
{
  # Split response at new line character into single rows
  rows <- split_into_lines(response)

  # Example row
  # "-rw-r--r--    1 9261     15101        9132 Mar 27 15:05"

  # Keep only the left-hand parts, representing eight columns (separated by one
  # or more spaces), from each row
  text <- kwb.utils::extractSubstring("^((([^ ]+) +){8})", rows, index = 1L)

  # Convert the text rows into a data frame of file properties (except name)
  info <- utils::read.table(
    text = text, stringsAsFactors = FALSE, col.names = c(
      "permissions", "links", "user", "group", "size", "month", "day",
      "year_or_time"
    )
  )

  # Set the filenames (right parts of the rows) in the returned info data frame
  kwb.utils::setColumns(dbg = FALSE, info, file = mapply(
    right, rows, nchar(rows) - nchar(text), USE.NAMES = FALSE
  ))
}

# empty_file_info --------------------------------------------------------------
empty_file_info <- function(full_info = TRUE)
{
  full_record <- data.frame(
    file = character(),
    is_directory = logical(),
    #type = character(),
    size = numeric(),
    permissions = character(),
    modification_time = character(),
    user = character(),
    group = character()
  )

  if (full_info) {

    full_record

  } else {

    full_record[, 1:2]
  }
}

# set_file ---------------------------------------------------------------------
set_file <- function(df, file)
{
  kwb.utils::setColumns(df, file = file, dbg = FALSE)
}

# get_file ---------------------------------------------------------------------
get_file <- function(df)
{
  kwb.utils::selectColumns(df, "file")
}

# row_represents_directory -----------------------------------------------------
row_represents_directory <- function(info)
{
  # Which files represent directories? If column "is_directory" is not yet
  # available, look for a "d" at the beginning of the permission strings
  # Extract permission strings (to check for the directory flag "d")
  kwb.utils::defaultIfNULL(
    info[["is_directory"]],
    grepl("^d", kwb.utils::selectColumns(info, "permissions"))
  )
}

# at_max_depth -----------------------------------------------------------------
at_max_depth <- function(depth, max_depth)
{
  ! is.na(max_depth) && (depth == max_depth)
}

# merge_url_lists --------------------------------------------------------------
merge_url_lists <- function(url_lists, directories, full_info)
{
  stopifnot(is.list(url_lists))

  # Prepare an empty result set
  empty_result <- empty_file_info(full_info)

  if (length(url_lists) == 0L) {
    return(empty_result)
  }

  # Merge the file lists returned for each directory
  files <- kwb.utils::excludeNULL(
    dbg = FALSE,
    lapply(seq_along(url_lists), FUN = function(i) {
      #i <- 1
      urls <- url_lists[[i]]
      if (nrow(urls) > 0L) {
        set_file(urls, paste0(directories[i], "/", get_file(urls)))
      } # else NULL implicitly
    })
  )

  if (length(files) == 0) {
    return(empty_result)
  }

  # Return the vector of files with an attribute "failed" holding the merged
  # URLs of directories that could not be read
  structure(bind_list_elements(files), failed = unlist(kwb.utils::excludeNULL(
    lapply(url_lists, attr, "failed"), dbg = FALSE
  )))
}
