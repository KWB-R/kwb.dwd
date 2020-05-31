# list_url_ --------------------------------------------------------------------

# In contrast to list_url(), this function always returns a data frame, at least
# with columns "file", "is_directory", if full_info = FALSE.
# @param depth for start depth when \code{recursive = TRUE}
# @param curl RCurl handle passed to \code{kwb.dwd:::try_to_get_url}
list_url_ <- function(
  url, recursive = TRUE, max_depth = 1, full_info = FALSE, ..., depth = 0,
  prob_mutate = 0
)
{
  if (FALSE) {
    kwb.utils::assignPackageObjects("kwb.dwd")
    kwb.utils::assignArgumentDefaults(list_url)
    kwb.utils::assignArgumentDefaults(list_url_)
    url <- kwb.dwd:::ftp_path_cdc("help/landing_pages")
    max_depth = 1;full_info=TRUE;set.seed(1)
  }

  # Check URL and append slash if necessary
  url <- assert_url(url)

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

    # Indicate directories with trailing slash
    info$file <- indicate_directories(get_info("file"), is_directory)

    return(info)
  }

  # If we arrive here, a recursive listing is requested. Number of directories
  n_directories <- sum(is_directory)

  # There must be directories if we arrive here!
  stopifnot(n_directories > 0L)

  # List the directories by calling this function recursively

  # URLs representing directories
  directories <- get_info("file")[is_directory]

  # List all directories
  url_lists <- lapply(seq_len(n_directories), function(i) {

    #i <- 1L
    # Show the progress
    cat(sprintf("%s%d/%d: ", space(depth), i, n_directories))

    # Recursive call of this function
    list_url_(
      url = paste0(url, directories[i]),
      recursive = recursive,
      max_depth = max_depth,
      full_info = full_info,
      ...,
      depth = depth + 1,
      prob_mutate = prob_mutate
    )
  })

  url_data <- merge_url_lists(url_lists, directories, full_info)

  # Merge files at this level with files in subdirectories. Return the sorted
  # file list with attribute "failed" if any directory URL could not be accessed
  structure(
    order_by(rbind(info = info[! is_directory, ], url_data), "file"),
    failed = attr(url_data, "failed")
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
  files <- silently_exclude_null(
    lapply(seq_along(url_lists), FUN = function(i) {
      #i <- 1
      urls <- url_lists[[i]]
      if (nrow(urls) > 0L) {
        kwb.utils::setColumns(dbg = FALSE, urls, file = paste0(
          directories[i], "/", kwb.utils::selectColumns(urls, "file")
        ))
      } # else NULL implicitly
    })
  )

  if (length(files) == 0) {
    return(empty_result)
  }

  # Return the vector of files with an attribute "failed" holding the merged
  # URLs of directories that could not be read
  structure(bind_list_elements(files), failed = collect_failed(url_lists))
}

# collect_failed ---------------------------------------------------------------
collect_failed <- function(x)
{
  stopifnot(is.list(x))

  unlist(silently_exclude_null(lapply(x, attr, "failed")))
}
