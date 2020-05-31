# list_contents ----------------------------------------------------------------
list_contents <- function(x = character(), full_info = FALSE, ...)
{
  # Return the empty
  if (length(x) == 0L) {
    return(empty_file_info(full_info))
  }

  # Check URL and append slash if necessary
  x <- assert_url(x)

  # Get a response from the FTP server
  response <- try_to_get_url(x, ...)
  #response <- try_to_get_url(x)

  # Convert response string to data frame
  info <- response_to_data_frame(response, full_info)

  # Return if info is empty. A response of NULL indicates that an error occurred
  # when reading from the URL. In this case, set the attribute "failed" to the
  # URL that failed to be accessed.
  structure(info, failed = if (is.null(response)) x)
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

  get_info <- function(x) kwb.utils::selectColumns(info, x)

  # Required column: is_directory
  info$is_directory <- grepl("^d", get_info("permissions"))

  if (! full_info) {
    return(get_info(c("file", "is_directory")))
  }

  #info$type <- ifelse(info$is_directory, "directory", "file")

  # Replace columns "year_or_time", "month", "day" with "modification_time".
  # Put the most important columns first.
  main_columns_first(simplify_time_info(info))
}

# empty_file_info --------------------------------------------------------------
empty_file_info <- function(full_info = TRUE)
{
  full_record <- kwb.utils::noFactorDataFrame(
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
