build_opts = c("--no-resave-data", "--no-manual")
remotes::install_github("kwb-r/kwb.dwd@dev", build_opts = build_opts)

# Compare URLs to Radolan files
# 1. expected URLs returned by kwb.dwd::get_radolan_urls()
# 2. existing URLs returned by kwb.dwd::list_url()

# Get lists of expected URLs
url_lists <- kwb.dwd::get_radolan_urls()

# Read the corresponding file lists from the ftp server
listed_urls <- lapply(url_lists, function(url_list) {
  base_url <- unique(dirname(dirname(url_list)))
  #files <- kwb.dwd::list_url(base_url, recursive = TRUE, n_trials = 3)
  #paste0(base_url, "/", files)
  kwb.dwd:::list_ftp_files_recursively(base_url, sleep_between_trials = 0.1)
})

# Missing files
lapply(stats::setNames(nm = names(url_lists)), function(key) {
  found <- url_lists[[key]] %in% listed_urls[[key]]
  url_lists[[key]][! found]
})

head(url_lists$hourly_historical_urls)
listed_urls$hourly_historical_urls

files <- kwb.dwd:::list_ftp_files_recursively(
  "ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/hourly/radolan/historical/bin",
  n_trials = 10, sleep_between_trials = 0.1, sleep_between_runs = 1
)

files <- kwb.dwd::list_url(
  "ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/hourly/radolan/historical",
  recursive = TRUE, max_depth = 1
)
