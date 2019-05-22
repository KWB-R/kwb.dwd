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
  files <- kwb.dwd::list_url(base_url, recursive = TRUE, n_attempts = 3)
  paste0(base_url, "/", files)
})

# Missing files
lapply(stats::setNames(nm = names(url_lists)), function(key) {
  found <- url_lists[[key]] %in% listed_urls[[key]]
  url_lists[[key]][! found]
})
