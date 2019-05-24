# list_ftp_files_recursively ---------------------------------------------------
list_ftp_files_recursively <- function(
  url, n_trials = 3, sleep_between_trials = 5, sleep_between_runs = 10, ...
)
{
  result <- list()

  on.exit(return(
    if (length(failed)) {
      merge_url_list(result)
    } else {
      paste0(assert_trailing_slash(url), sort(unlist(result)))
    }
  ))

  failed <- url

  while (length(failed)) {

    url_list <- lapply(
      failed,
      list_url,
      recursive = TRUE,
      n_trials = n_trials,
      sleep_time = sleep_between_trials,
      ...
    )

    urls <- merge_url_list(url_list)
    (failed <- attr(urls, "failed"))

    result[[length(result) + 1]] <- structure(urls, failed = NULL)

    if (length(failed)) {
      message("Trying to read ", length(failed), " failing URLs again...")
      Sys.sleep(sleep_between_runs)
    }
  }
}

# merge_url_list ---------------------------------------------------------------
merge_url_list <- function(url_list)
{
  structure(
    if (length(url_list)) unlist(url_list) else character(),
    failed = unlist(lapply(url_list, attr, "failed"))
  )
}

# list_ftp_files_recursively_2 ---------------------------------------------------
list_ftp_files_recursively_2 <- function(
  url, n_trials = 3, sleep_between_trials = 5, sleep_between_runs = 10, ...
)
{
  result <- list()
  on.exit(return(sort(unlist(result))))

  failed <- url

  while (length(failed)) {

    url_list <- lapply(
      failed,
      list_url,
      recursive = TRUE,
      n_trials = n_trials,
      sleep_time = sleep_between_trials,
      ...
    )

    full_urls <- merge_url_list_2(url_list, failed)

    result[[length(result) + 1]] <- structure(full_urls, failed = NULL)

    failed <- attr(full_urls, "failed")

    if (length(failed)) {
      message("Trying to read ", length(failed), " failing URLs again...")
      Sys.sleep(sleep_between_runs)
    }
  }
}

# merge_url_list_2 ---------------------------------------------------------------
merge_url_list_2 <- function(url_list, base_urls)
{
  merged <- if (length(url_list)) {
    unlist(lapply(seq_along(url_list), function(i) {
      paste0(assert_trailing_slash(base_urls[i]), url_list[[i]])
    }))
  } else {
    character()
  }

  structure(merged, failed = unlist(lapply(url_list, attr, "failed")))
}
