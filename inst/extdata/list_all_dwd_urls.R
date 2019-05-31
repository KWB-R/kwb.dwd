if (FALSE)
{
  base_url <- "ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/hourly/radolan"
  base_url <- "ftp://ftp-cdc.dwd.de/pub/CDC"

  x <- kwb.dwd::list_url(url = "ftp://ftp-cdc.dwd.de/pub/CDC/derived_germany/techn/monthly/cooling_degreehours/cdh_16", recursive = TRUE)
  x <- kwb.dwd::list_url(url = "ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/hourly/radolan")
  x <- kwb.dwd::list_url(url = base_url, recursive = TRUE)

  system.time(result_urls <- kwb.dwd:::list_ftp_files_recursively(
    url = base_url,
    n_trials = 5,
    sleep_between_trials = 0.1,
    sleep_between_runs = 1
  ))

  # User  System verstrichen
  # 37.86   4.30      795.86

  subdir_matrix <- kwb.file:::to_subdir_matrix(result_urls)

  View(subdir_matrix)
}

# Save all URLs in a text file -------------------------------------------------
if (FALSE)
{
  file <- format(Sys.time(), format = "inst/extdata/urls_dwd_%Y%m%d_%H%M.txt")

  stopifnot(basename(getwd()) == "kwb.dwd")

  writeLines(result_urls, file)
}

# ------------------------------------------------------------------------------
if (FALSE)
{
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

# Provide all URLs as data in the package --------------------------------------
if (FALSE)
{
  files <- dir("inst/extdata", "^urls_dwd", full.names = TRUE)

  # Read all files
  urls_list <- lapply(files, readLines)

  # Are the URLs unsorted?
  sapply(urls_list, is.unsorted)

  a <- urls_list[[1]]
  b <- urls_list[[2]]

  head(a[!a %in% b])
  head(b[!b %in% a])

  # Select the URLs of one file only
  urls <- urls_list[[2]]

  urls[duplicated(urls)]

  url_data <- kwb.utils::noFactorDataFrame(
    path = dirname(urls),
    file = basename(urls)
  )

  View(url_data)

  tail(sort(table(url_data$file)))
}
