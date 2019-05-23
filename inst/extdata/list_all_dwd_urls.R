if (FALSE)
{
  base_url <- "ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/hourly/radolan"
  base_url <- "ftp://ftp-cdc.dwd.de/pub/CDC"

  #urls <- kwb.dwd::list_url(base_url, recursive = TRUE, n_attempts = 1)
  #failed <- attr(urls, "failed")

  result_urls <- kwb.dwd:::list_ftp_files_recursively(base_url)

  length(result_urls)

  writeLines(result_urls)

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
