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
  kwb.dwd::list_url(base_url, recursive = TRUE, full_names = TRUE)
})

# Missing files
lapply(stats::setNames(nm = names(url_lists)), function(key) {
  found <- url_lists[[key]] %in% listed_urls[[key]]
  url_lists[[key]][! found]
})

# Read all URLs available at DWD as they were saved to file
file_urls <- rev(dir("inst/extdata/", "^urls_dwd", full.names = TRUE))[1]
saved_urls <- readLines(file_urls)
length(saved_urls)

# Are the expected URLs actually available?
table(url_lists$daily_historical_urls %in% saved_urls)
table(url_lists$hourly_historical_urls %in% saved_urls)
