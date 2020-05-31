# list_url_ --------------------------------------------------------------------

# In contrast to list_url(), this function always returns a data frame, at least
# with columns "file", "is_directory", if full_info = FALSE.
# @param depth for start depth when \code{recursive = TRUE}
list_url_ <- function(
  url, recursive = TRUE, max_depth = 1, full_info = FALSE, ..., depth = 0,
  prob_mutate = 0
)
{
  # kwb.utils::assignPackageObjects("kwb.dwd")
  # kwb.utils::assignArgumentDefaults(list_url)
  # kwb.utils::assignArgumentDefaults(list_url_)
  # url <- kwb.dwd:::ftp_path_cdc("help/landing_pages")
  # max_depth = 1;full_info=TRUE;set.seed(1)

  # Call the domain specific function list_contents(). The function is expected
  # to set the attribute "failed" to the given URL in case that the URL failed
  # to be accessed.
  info <- list_contents(mutate_or_not(url, prob_mutate), full_info, ...)
  #info <- list_contents(mutate_or_not(url, prob_mutate), full_info)

  # Helper function to get an info column
  get_info <- function(x) kwb.utils::selectColumns(info, x)

  # Which files represent directories?
  is_directory <- get_info("is_directory")

  # Return the file list if no recursive listing is requested or if we are
  # already at maximum depth or if there are no directories. The function is
  # also returned from if info is empty (! any(is_directory) is TRUE).
  if (! recursive || at_max_depth(depth, max_depth) || ! any(is_directory)) {
    return(info)
  }

  # URLs representing directories
  directories <- get_info("file")[is_directory]

  # Number of directories
  n_directories <- length(directories)

  # There must be directories if we arrive here!
  stopifnot(n_directories > 0L)

  # Indices to loop through
  indices <- stats::setNames(seq_along(directories), directories)

  # List all directories by calling this function recursively
  subdir_infos <- lapply(indices, function(i) {

    #i <- 1L

    # Show the progress
    cat(sprintf("%s%d/%d: ", space(depth), i, n_directories))

    # Recursive call of this function
    list_url_(
      url = paste0(kwb.utils::assertFinalSlash(url), directories[i]),
      recursive = recursive,
      max_depth = max_depth,
      full_info = full_info,
      ...,
      depth = depth + 1,
      prob_mutate = prob_mutate
    )
  })

  # Merge data frames with info on files in subdirectories
  subdir_info <- merge_file_infos(subdir_infos, full_info)

  # Prepend info on files at this level
  result <- rbind(info[! is_directory, ], subdir_info)

  # Collect information on URLs that failed to be accessed
  failed <- c(attr(info, "failed"), attr(subdir_info, "failed"))

  # Return the sorted file information with newly composed attribute "failed"
  structure(order_by(result, "file"), failed = failed)
}

# at_max_depth -----------------------------------------------------------------
at_max_depth <- function(depth, max_depth)
{
  ! is.na(max_depth) && (depth == max_depth)
}

# merge_file_infos -------------------------------------------------------------
merge_file_infos <- function(file_infos, full_info)
{
  stopifnot(is.list(file_infos))

  # Keep only non-empty data frames
  dfs <- file_infos[sapply(file_infos, nrow) > 0L]

  # Function to prepend a parent name "p" to column "file" in data frame "df"
  prepend_parent <- function(df, p) {
    parent <- kwb.utils::assertFinalSlash(p)
    child <- kwb.utils::selectColumns(df, "file")
    kwb.utils::setColumns(df, file = paste0(parent, child), dbg = FALSE)
  }

  # Prepend the parent names to the filenames for the remaining data frames
  result <- do.call(rbind, mapply(
    prepend_parent, dfs, names(dfs), SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))

  # If the result is NULL (no data frames to loop through) set the result to the
  # empty file info record
  result <- kwb.utils::defaultIfNULL(result, empty_file_info(full_info))

  # Collect the information on URLs that could not be listed
  failed <- unlist(silently_exclude_null(lapply(file_infos, attr, "failed")))

  # Merge the file lists returned for each directory
  # Return the vector of files with an attribute "failed" holding the merged
  # URLs of directories that could not be read
  structure(result, failed = failed)
}
