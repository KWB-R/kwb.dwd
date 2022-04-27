# List all available files -----------------------------------------------------
if (FALSE)
{
  base_url <- kwb.dwd:::ftp_path_cdc()

  kwb.utils::clearConsole()

  system.time({
    x <- kwb.dwd::list_url(base_url, recursive = TRUE)
    result_urls <- paste0(kwb.utils::assertFinalSlash(base_url), x)
  })

  #   User      System verstrichen
  # 184.55        4.79      785.71

  subdir_matrix <- kwb.file:::to_subdir_matrix(result_urls)
  head(subdir_matrix)

  dim(subdir_matrix)
  # [1] 700830     13

  dwd_files <- x

  usethis::use_data(dwd_files, overwrite = TRUE)
}

# Save all URLs in a text file -------------------------------------------------
if (FALSE)
{
  file <- format(Sys.time(), format = "inst/extdata/urls_dwd_%Y%m%d_%H%M.txt")

  stopifnot(basename(getwd()) == "kwb.dwd")

  writeLines(result_urls, file)
}

# List all paths below grids_germany -------------------------------------------
if (FALSE)
{
  base_url <- kwb.dwd:::ftp_path_cdc("grids_germany")

  url_hourly <- paste0(base_url, "/hourly")
  url_monthly <- paste0(base_url, "/monthly")

  all_urls_hourly <- kwb.dwd::list_url(url_hourly, recursive = TRUE)
  all_urls_monthly <- kwb.dwd::list_url(url_monthly, recursive = TRUE)

  urls <- kwb.dwd::list_url(url = base_url, recursive = TRUE)

  length(urls)

  lapply(attr(urls, "failed"), kwb.dwd::list_url, recursive = TRUE)

  View(data.frame(url = urls))
}

# Provide all URLs as data in the package --------------------------------------
if (FALSE)
{
  # Get most current two files
  files <- rev(dir("inst/extdata", "^urls_dwd", full.names = TRUE))[1:2]

  # Read all files
  urls_list <- lapply(files, readLines)

  # Are the URLs unsorted?
  sapply(urls_list, is.unsorted)

  a <- urls_list[[1]]
  b <- urls_list[[2]]

  length(a[! a %in% b])
  length(b[! b %in% a])

  a <- gsub("opendata.dwd.de/climate_environment", "ftp-cdc.dwd.de/pub", a)

  n <- 1000
  table(head(a, n) == head(b, n))

  kwb.utils::compareSets(a, b)

  # Select the URLs of one file only
  urls <- c
  urls <- urls_list[[1]]

  urls[duplicated(urls)]

  dwd_files <- kwb.utils::noFactorDataFrame(
    path = dirname(urls),
    file = basename(urls)
  )

  opendata_files <- dwd_files

  View(dwd_files)

  # Most frequent file names
  tail(sort(table(dwd_files$file)))

  usethis::use_data(opendata_files)
}
