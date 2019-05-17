# read_relevant_years_radolan --------------------------------------------------

#' Reads Relevant Radolan Data from rst Database
#'
#' This function reads in relevant radolan data from rst database based on used
#' input data
#'
#' @param path path to rst database (folder)
#' @param years vector of year numbers for which to read Radolan data files
#' @export
#'
read_relevant_years_radolan <- function(path, years)
{
  stopifnot(is.numeric(years))
  stopifnot(all(years >= 2000 & years < 2100))
  stopifnot(file.exists(path))

  # Define a pattern describing the names of files relating to the given years
  two_digit_years <- unique(years - 2000)
  pattern <- sprintf("^(%s).*\\.gri$", paste(two_digit_years, collapse = "|"))

  # Get paths of files matching the pattern
  files <- list.files(path, pattern, full.names = TRUE, recursive = TRUE)

  if (length(files) == 0) {

    message(sprintf("No files found that match '%s'", pattern))
    return()
  }

  # Extract date strings (yymmdd) from file names
  date_strings <- substr(basename(files), 1, 6)

  # Load radolan files for relevant years and label layers with date strings
  stats::setNames(raster::stack(files), date_strings)
}
