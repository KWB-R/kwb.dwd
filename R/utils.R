# assert_url -------------------------------------------------------------------
assert_url <- function(url, final_slash = TRUE)
{
  stopifnot(is.character(url))
  stopifnot(length(url) == 1)

  # Append slash if necessary
  if (final_slash) {

    kwb.utils::assertFinalSlash(url)

  } else {

    url
  }
}

# cat0 -------------------------------------------------------------------------
cat0 <- function(...)
{
  cat(paste0(...))
}

# cat_progress -----------------------------------------------------------------
cat_progress <- function(i, n, success = TRUE, chars = c(".", "x"))
{
  space <- function(n) kwb.utils::space(n, tabLength = 1L)
  back <- kwb.utils::backspace

  if (i == 0L) {
    cat0("[", space(n), "]")
  } else {
    cat0(back(n - i + 2L), chars[success + 1L], space(n - i), "]")
  }
}

# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(..., call. = FALSE)
}

# date_in_bathing_season -------------------------------------------------------
date_in_bathing_season <- function(x)
{
  months <- lubridate::month(x)

  months >= 5 & months < 10
}

# filter_zipped_esri_ascii_grids -----------------------------------------------
filter_zipped_esri_ascii_grids <- function(x)
{
  grep("\\.asc\\.gz$", x, value = TRUE)
}

# get_date_time_from_bin_filename ----------------------------------------------
get_date_time_from_bin_filename <- function(x)
{
  format <- "raa01-sf_10000-%y%m%d%H%M-dwd---bin"
  times <- as.POSIXct(basename(x), format = format, tz = "UTC")

  is_na <- is.na(times)

  if (any(is_na)) {
    warning(
      "For ", sum(is_na), " files, the date and time could not be determined: ",
      kwb.utils::stringList(utils::head(x[is_na])), call. = FALSE
    )
  }

  times
}

# get_element_or_stop ----------------------------------------------------------
get_element_or_stop <- function(x, element, name = deparse(substitute(element)))
{
  x[safe_element(element, names(x), name)]
}

# indicate_directories ---------------------------------------------------------
indicate_directories <- function(x, is_directory)
{
  if (length(x) == 0L) {
    return(x)
  }

  x[is_directory] <- kwb.utils::assertFinalSlash(x[is_directory])
  x
}

# is_empty ---------------------------------------------------------------------
is_empty <- function(x)
{
  (is.data.frame(x) && nrow(x) == 0L) || (length(x) == 0L)
}

# list_files_in_zip_files ------------------------------------------------------
list_files_in_zip_files <- function(zip_files, dbg = TRUE)
{
  do.call(rbind, lapply(zip_files, function(x) {
    kwb.utils::catAndRun(
      messageText = paste("Getting names of files in", x),
      dbg = dbg,
      expr = kwb.utils::noFactorDataFrame(
        zip_file = basename(x),
        file = utils::untar(x, list = TRUE)
      )
    )
  }))
}

# list_zipped_esri_ascii_grids -------------------------------------------------

#' Get URLs of Files in Zipped ESRI-ascii-grid Format
#'
#' @param base_url URL from which to start listing (recursively by default)
list_zipped_esri_ascii_grids <- function(base_url, recursive = TRUE)
{
  # List data files
  relative_urls <- list_url(base_url, recursive = recursive)

  # Provide full paths to zipped files in ESRI-ascii-grid-format
  file.path(base_url, filter_zipped_esri_ascii_grids(relative_urls))
}

# month_numbers ----------------------------------------------------------------
month_numbers <- function()
{
  list(
    Jan = 1L, Feb = 2L, Mar = 3L, Apr = 04L, May = 05L, Jun = 06L,
    Jul = 7L, Aug = 8L, Sep = 9L, Oct = 10L, Nov = 11L, Dec = 12L
  )
}

# month_sequence ---------------------------------------------------------------
month_sequence <- function(start, end)
{
  to_date <- function(x) lubridate::ymd(paste0(x, "-01"))

  seq(to_date(start), to_date(end), by = 'months')
}

# safe_element -----------------------------------------------------------------
safe_element <- function(element, elements, name = deparse(substitute(element)))
{
  if (! element %in% elements) clean_stop(sprintf(
    "%s ('%s') must be one of %s",
    name, element, kwb.utils::stringList(elements)
  ))

  element
}

# split_into_lines -------------------------------------------------------------
split_into_lines <- function(x)
{
  stopifnot(is.character(x), length(x) == 1L)
  strsplit(x, "\r?\n")[[1]]
}
