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
      paste0(url, "/", sort(unlist(result)))
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
