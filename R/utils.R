# add_attributes ---------------------------------------------------------------
add_attributes <- function(x, attrs)
{
  stopifnot(is.list(attrs))

  do.call(structure, c(list(x), attrs))
}

# assert_all_ending_with -------------------------------------------------------------
assert_all_ending_with <- function(x, suffix)
{
  stopifnot(all(endsWith(x, suffix)))
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

# contains_file ----------------------------------------------------------------
contains_file <- function(path, pattern)
{
  length(dir(path, pattern)) > 0L
}

# copy_file --------------------------------------------------------------------
copy_file <- function(from, to)
{
  success <- file.copy(from = from, to = to)

  if (!all(success)) {
    kwb.utils::stopFormatted("Could not copy %s to %s", from, to)
  }
}

# date_in_bathing_season -------------------------------------------------------
#' @importFrom lubridate month
date_in_bathing_season <- function(x)
{
  # May to September
  lubridate::month(x) %in% 5:9
}

# download_dir -----------------------------------------------------------------
download_dir <- function(...)
{
  ifelse(on_windows(), "~/../Downloads", "~/Downloads") %>%
    path.expand() %>%
    file.path(...) %>%
    kwb.utils::createDirectory(dbg = FALSE)
}

# download_if_not_there --------------------------------------------------------
download_if_not_there <- function(
    url,
    file = file.path(target_dir, basename(url)),
    target_dir = download_dir(),
    quiet = FALSE,
    mode = "w",
    timeout = getOption("timeout")
)
{

  if (file.exists(file)) {

    kwb.utils::catIf(!quiet, "\nFile already there:", file, "\n")

  } else {

    # Temporarily set the timeout option
    old_options <- options(timeout = timeout)
    on.exit(options(old_options))

    result <- kwb.utils::catAndRun(
      sprintf("\nDownloading\n  %s\nto\n  %s", url, file),
      dbg = !quiet,
      expr = try(download.file(
        url = url,
        destfile = file,
        method = "auto",
        quiet = TRUE,
        mode = mode
      ))
    )

    if (kwb.utils::isTryError(result) || !identical(result, 0L)) {
      if (file.exists(file)) {
        if (!identical(unlink(file), 0L)) {
          message("Could not delete incompletely downloaded file: ", file)
        }
      }
      kwb.utils::stopFormatted(
        "Could not download %s within %d seconds.", url, timeout
      )
    }

  }

  file
}

# filter_by_extension ----------------------------------------------------------
filter_by_extension <- function(x, extension)
{
  x[endsWith(tolower(x), tolower(extension))]
}

# filter_by_month_range --------------------------------------------------------
filter_by_month_range <- function(urls, from = NULL, to = NULL)
{
  if (length(urls) == 0L) {
    return(urls)
  }

  extract_yyyymm <- function(x) gsub("^.*(\\d{6}).*$", "\\1", basename(x))

  from <- kwb.utils::defaultIfNULL(from, extract_yyyymm(urls[1L]))
  to <- kwb.utils::defaultIfNULL(to, extract_yyyymm(urls[length(urls)]))

  pattern <- month_range_pattern(from, to)

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

# get_full_extension -----------------------------------------------------------
get_full_extension <- function(x)
{
  parts <- lapply(strsplit(x, "\\."), rev)

  result <- extension <- character(length(x))

  n_parts <- lengths(parts)

  selected <- n_parts > 1L
  result[selected] <- sapply(parts[selected], "[", 1L)

  selected <- n_parts > 2L
  extension[selected] <- sapply(parts[selected], "[", 2L)

  extension[!looks_like_file_extension(extension)] <- ""
  selected <- extension != ""
  result[selected] <- paste0(extension[selected], ".", result[selected])

  result
}

# get_relative_path ------------------------------------------------------------
get_relative_path <- function(
    file,
    base_dir = kwb.utils::getAttribute(file, "base_dir")
)
{
  remove_left(file, nchar(kwb.utils::assertFinalSlash(base_dir)))
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

# last_month -------------------------------------------------------------------
last_month <- function(format = "%Y%m")
{
  format.Date(Sys.Date() - 31L, format)
}

# list_files_in_zip_files ------------------------------------------------------
#' @importFrom kwb.utils catAndRun noFactorDataFrame
#' @importFrom utils untar
list_files_in_zip_files <- function(zip_files, dbg = TRUE)
{
  list_files <- function(file) {
    kwb.utils::catAndRun(
      messageText = paste("Getting names of files in", file),
      dbg = dbg,
      expr = kwb.utils::noFactorDataFrame(
        zip_file = basename(file),
        file = list_zipped_files(file)
      )
    )
  }

  lapply(zip_files, list_files) %>%
    do.call(what = rbind) %>%
    kwb.utils::orderBy(c("zip_file", "file"))
}

# list_zipped_files ------------------------------------------------------------
list_zipped_files <- function(file)
{
  result <- utils::untar(kwb.utils::safePath(file), list = TRUE)

  if (!is.null(attr(result, "status"))) {

    tar_message <- grep("tar.exe: ", result, value = TRUE) %>%
      gsub(pattern = "^.*(tar.exe: .*)$", replacement = "\\1")

    clean_stop(
      "There was a warning listing the files in\n  ", file, ".\n",
      "The file seems to be corrupt.\n",
      paste(tar_message, collapse = "\n")
    )
  }

  result
}

# looks_like_file_extension ----------------------------------------------------
looks_like_file_extension <- function(x)
{
  !grepl("(^[0-9]+$)|[_-]", x)
}

# month_range_pattern ----------------------------------------------------------
month_range_pattern <- function(from, to)
{
  paste(month_sequence(from, to, simple = TRUE), collapse = "|")
}

# month_sequence ---------------------------------------------------------------
#' @importFrom lubridate ymd
month_sequence <- function(start, end, simple = FALSE)
{
  if (simple) {

    as_date <- function(x) as.Date(paste0(x, "01"), format = "%Y%m%d")
    unique(format(seq(as_date(start), as_date(end), 1L), "%Y%m"))

  } else {

    as_date <- function(x) lubridate::ymd(paste0(x, "-01"))
    seq(as_date(start), as_date(end), by = 'months')
  }
}

# on_windows -------------------------------------------------------------------
on_windows <- function()
{
  Sys.info()[["sysname"]] == "Windows"
}

# open_for_reading_in_binary_mode ----------------------------------------------
open_for_reading_in_binary_mode <- function(file)
{
  if (endsWith(file, ".gz")) {

    base::gzfile(file, "rb")

  } else {

    base::file(file, "rb")
  }
}

# remove_left ------------------------------------------------------------------
remove_left <- function(x, n)
{
  n_char <- nchar(x)
  stopifnot(all(n_char >= n))
  substr(x, n + 1L, n_char)
}

# remove_protocol --------------------------------------------------------------
remove_protocol <- function(x)
{
  gsub("^[^/]+://", "", x)
}


# remove_right -----------------------------------------------------------------
remove_right <- function(x, n)
{
  n_char <- nchar(x)
  stopifnot(all(n_char >= n))
  substr(x, 1L, n_char - n)
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
#' Path to Permanent Temporary Directory
#'
#' @param \dots parts of the path after `<TEMP_DIR>/R_kwb.dwd/`, passed to
#'   [file.path]] where `<TEMP_DIR>` is either the value of environment variable
#'   `TEMP` (if set) or `TMP` (if set) or the result of calling [tempdir].
#' @param template optional. If given, it is assumed to be a path to a file. The
#'   name of the file without file name extension is then used as folder name
#'   below `<TEMP_DIR>/R_kwb.dwd/`.
#' @param create logical indicating whether or not to create the folder if it
#'   does not yet exist. Defaults to `TRUE`.
#' @param dbg logical indicating whether or not to print debug messages
#' @return The function returns the path to the temporary folder specified.
#' @export
temp_dir <- function(..., template = NULL, create = TRUE, dbg = FALSE)
{
  dot_args <- list(...)

  # If no template (path) is given, use the arguments in ... as sub directory
  # names. Otherwise, use the base file name of the template without the file
  # name extension as sub directory name
  args <- if (is.null(template)) {

    dot_args

  } else if (!is_empty(dot_args)) {

    clean_stop(
      "Further arguments to temp_dir() not allowed if 'template' is given."
    )

  } else {

    list(kwb.utils::removeExtension(basename(template)))
  }

  tmp_dir <- Sys.getenv("TEMP", Sys.getenv("TMP", tempdir()))

  path <- do.call(file.path, c(list(tmp_dir, "R_kwb.dwd"), args))

  if (create) {
    kwb.utils::createDirectory(path, dbg = dbg)
  }

  path
}

# url_subdirs_containing_files_with_extension ----------------------------------
url_subdirs_containing_files_with_extension <- function(url, extension)
{
  subdir_urls <- list_url(url, full_names = TRUE)

  found <- sapply(subdir_urls, function(subdir_url) {
    any(endsWith(list_url(subdir_url, recursive = TRUE), extension))
  })

  basename(subdir_urls[found])
}
