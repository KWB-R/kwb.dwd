# assert_ending_gz -------------------------------------------------------------
assert_ending_gz <- function(x)
{
  stopifnot(all(endsWith(x, ".gz")))
  invisible(x)
}

# assert_url -------------------------------------------------------------------
#' @importFrom kwb.utils assertFinalSlash
assert_url <- function(url, final_slash = TRUE)
{
  stopifnot(is.character(url))
  stopifnot(length(url) == 1L)

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
#' @importFrom kwb.utils backspace space
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
#' @importFrom lubridate month
date_in_bathing_season <- function(x)
{
  # May to September
  lubridate::month(x) %in% 5:9
}

# extract_yyyymm ---------------------------------------------------------------
extract_yyyymm <- function(x)
{
  gsub("^.*(\\d{6}).*$", "\\1", basename(x))
}

# filter_by_extension ----------------------------------------------------------
filter_by_extension <- function(x, extension)
{
  x[endsWith(x, extension)]
}

# filter_by_extension_asc_gz ---------------------------------------------------
filter_by_extension_asc_gz <- function(x)
{
  filter_by_extension(x, ".asc.gz")
}

# filter_by_extension_tgz ------------------------------------------------------
filter_by_extension_tgz <- function(x)
{
  filter_by_extension(x, ".tgz")
}

# filter_by_month_range --------------------------------------------------------
filter_by_month_range <- function(urls, from = NULL, to = NULL)
{
  if (length(urls) == 0L) {
    return(urls)
  }

  from <- kwb.utils::defaultIfNULL(from, extract_yyyymm(urls[1L]))
  to <- kwb.utils::defaultIfNULL(to, extract_yyyymm(urls[length(urls)]))

  pattern <- paste(month_sequence_simple(from, to), collapse = "|")

  urls[grep(pattern, urls)]
}

# get_date_time_from_bin_filename ----------------------------------------------
#' @importFrom kwb.utils stringList
#' @importFrom utils head
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
#' @importFrom kwb.utils assertFinalSlash
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

# last_month_as_yyyymm ---------------------------------------------------------
last_month_as_yyyymm <- function()
{
  format(Sys.Date() - 31L, "%Y%m")
}

# list_files_in_zip_files ------------------------------------------------------
#' @importFrom kwb.utils catAndRun noFactorDataFrame
#' @importFrom utils untar
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

# list_monthly_grids_germany_asc_gz -------------------------------------------------

#' Get URLs to Monthly Grids in Zipped ESRI-ascii-grid Format
#'
#' @param variable variable for which to look for URLs. Must be one of
#'   \code{kwb.dwd::list_url(kwb.dwd:::ftp_path_monthly_grids())}
#' @param from optional. First month to be considered, as "yyyymm" string
#' @param to optional. Last month to be considered, as "yyyymm" string
#' @param recursive whether to list files recursively. Default: \code{TRUE}
list_monthly_grids_germany_asc_gz <- function(
  variable, from = NULL, to = NULL, recursive = TRUE
)
{
  base_url <- ftp_path_monthly_grids(variable)

  # List data files
  relative_urls <- base_url %>%
    list_url(recursive = recursive) %>%
    filter_by_extension_asc_gz() %>%
    filter_by_month_range(from, to)

  # Provide full paths to zipped files in ESRI-ascii-grid-format
  file.path(base_url, relative_urls)
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
#' @importFrom lubridate ymd
month_sequence <- function(start, end)
{
  to_date <- function(x) lubridate::ymd(paste0(x, "-01"))

  seq(to_date(start), to_date(end), by = 'months')
}

# month_sequence_simple --------------------------------------------------------
month_sequence_simple <- function(from, to)
{
  as_date <- function(x) as.Date(paste0(x, "01"), format = "%Y%m%d")

  unique(format(seq(as_date(from), as_date(to), 1L), "%Y%m"))
}

# safe_element -----------------------------------------------------------------
#' @importFrom kwb.utils stringList
safe_element <- function(element, elements, name = deparse(substitute(element)))
{
  if (! element %in% elements) clean_stop(sprintf(
    "%s ('%s') must be one of %s",
    name, element, kwb.utils::stringList(elements)
  ))

  element
}

# temp_dir ---------------------------------------------------------------------
temp_dir <- function(...)
{
  path <- file.path(Sys.getenv("TEMP"), "R_kwb.dwd", ...)
  kwb.utils::createDirectory(path, dbg = FALSE)
}
