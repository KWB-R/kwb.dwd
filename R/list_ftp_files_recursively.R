# list_ftp_files_recursively ---------------------------------------------------
list_ftp_files_recursively <- function(
  url, n_trials = 3, sleep_between_trials = 5, sleep_between_runs = 10
)
{
  result <- list()
  failed <- url

  while (length(failed)) {

    url_list <- lapply(
      failed,
      kwb.dwd::list_url,
      recursive = TRUE,
      n_trials = n_trials,
      sleep_time = sleep_between_trials
    )

    urls <- merge_url_list(url_list)
    (failed <- attr(urls, "failed"))

    result[[length(result) + 1]] <- structure(urls, failed = NULL)

    if (length(failed)) {
      message("Trying to read ", length(failed), " failing URLs again...")
      Sys.sleep(sleep_between_runs)
    }
  }

  paste0(url, "/", sort(unlist(result)))
}

# merge_url_list ---------------------------------------------------------------
merge_url_list <- function(url_list)
{
  failed <- unlist(lapply(url_list, attr, "failed"))
  structure(unlist(url_list), failed = failed)
}
