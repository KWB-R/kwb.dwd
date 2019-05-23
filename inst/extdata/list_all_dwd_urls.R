if (FALSE)
{
  base_url <- "ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/hourly/radolan"
  base_url <- "ftp://ftp-cdc.dwd.de/pub/CDC"

  #urls <- kwb.dwd::list_url(base_url, recursive = TRUE, n_attempts = 1)
  #failed <- attr(urls, "failed")

  result_urls <- kwb.dwd:::list_ftp_files_recursively(
    base_url, n_trials = 5, sleep_between_trials = 0.1, sleep_between_runs = 1,
    max_depth = 2
  )

  subdir_matrix <- kwb.file:::to_subdir_matrix(result_urls)

  View(subdir_matrix)

  # Trying to read 6 failing URLs again...
  # ftp://ftp-cdc.dwd.de/pub/CDC/derived_germany/techn/monthly/cooling_degreehours/cdh_13/historical/: ok.
  # ftp://ftp-cdc.dwd.de/pub/CDC/derived_germany/techn/monthly/cooling_degreehours/cdh_16/historical/: ok.
  # ftp://ftp-cdc.dwd.de/pub/CDC/derived_germany/techn/monthly/cooling_degreehours/cdh_16/recent/: ok.
  # ftp://ftp-cdc.dwd.de/pub/CDC/derived_germany/techn/monthly/cooling_degreehours/cdh_18/historical/: ok.
  # ftp://ftp-cdc.dwd.de/pub/CDC/derived_germany/techn/monthly/cooling_degreehours/cdh_18/recent/: ok.

  url <- "ftp://ftp-cdc.dwd.de/pub/CDC/derived_germany/techn/monthly/cooling_degreehours/cdh_13/historical/"
  kwb.dwd::list_url(url)
  RCurl::getURL(url)

  length(result_urls)

  file <- format(Sys.time(), format = "inst/extdata/urls_dwd_%Y%m%d_%H%M.txt")

  getwd()

  writeLines(result_urls, file)

  urls_list

  url_hourly <- paste0(base_url, "/hourly")
  url_monthly <- paste0(base_url, "/monthly")

  all_urls_hourly <- list_url(url_hourly, recursive = TRUE, n_attempts = 8)
  all_urls_monthly <- list_url(url_monthly, recursive = TRUE, n_attempts = 8)

  urls <- kwb.dwd::list_url(url = base_url, recursive = TRUE, n_attempts = 10)

  length(urls)

  lapply(attr(urls, "failed"), kwb.dwd::list_url, recursive = TRUE)

  View(data.frame(url = urls))
}
