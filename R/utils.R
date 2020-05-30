# back -------------------------------------------------------------------------
back <- function(n)
{
  repeated("\b", n)
}

# bind_list_elements -----------------------------------------------------------
bind_list_elements <- function(x)
{
  stopifnot(is.list(x))

  if (all(sapply(x, inherits, "data.frame"))) {

    do.call(rbind, x)

  } else {

    unlist(x)
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
  space_function <- function(n) repeated(" ", n)

  if (i == 0) {
    cat0("[", space_function(n), "]")
  } else {
    cat0(back(n - i + 2), chars[success + 1], space_function(n - i), "]")
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

# repeated ---------------------------------------------------------------------
repeated <- function(x, n)
{
  paste(rep(x, n), collapse = "")
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

# sort_or_order ----------------------------------------------------------------
sort_or_order <- function(x, by = NULL)
{
  if (is.data.frame(x)) {

    kwb.utils::resetRowNames(x[order(kwb.utils::selectColumns(x, by)), ])

  } else {

    sort(x)
  }
}
