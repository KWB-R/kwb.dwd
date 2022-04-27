# List all available files and create path info dataset in the package ---------
if (FALSE)
{
  base_url <- kwb.dwd:::ftp_path_cdc()

  kwb.utils::clearConsole()

  system.time({
    dwd_files <- kwb.dwd::list_url(base_url, recursive = TRUE, full_info = TRUE)
  })

  #   User      System verstrichen
  # 184.55        4.79      785.71

  # Save dataset in the package
  usethis::use_data(dwd_files, overwrite = TRUE)
}

# Save all URLs in a text file -------------------------------------------------
if (FALSE)
{
  result_urls <- paste0(kwb.utils::assertFinalSlash(base_url), dwd_files$file)

  file <- format(Sys.time(), format = "inst/extdata/urls_dwd_%Y%m%d_%H%M.txt")

  stopifnot(basename(getwd()) == "kwb.dwd")

  writeLines(result_urls, file)
}

# Split URLs into matrix of subdirectory names ---------------------------------
if (FALSE)
{
  subdir_matrix <- kwb.file::to_subdir_matrix(result_urls)
  head(subdir_matrix)
  dim(subdir_matrix)
  # [1] 700830     13
}

# List all paths below grids_germany -------------------------------------------
if (FALSE)
{
  base_url <- kwb.dwd:::ftp_path_cdc("grids_germany")

  url_hourly <- paste0(base_url, "/hourly/radolan/reproc")
  url_monthly <- paste0(base_url, "/monthly")

  all_urls_hourly <- kwb.dwd::list_url(url_hourly, recursive = TRUE, full_info = TRUE)
  all_urls_monthly <- kwb.dwd::list_url(url_monthly, recursive = TRUE)

  urls <- kwb.dwd::list_url(url = base_url, recursive = TRUE)

  length(urls)

  lapply(attr(urls, "failed"), kwb.dwd::list_url, recursive = TRUE)

  View(data.frame(url = urls))
}

# Compare latest path lists ----------------------------------------------------
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
}
