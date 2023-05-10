# download_and_extract_hourly_radolan_historical_bin ---------------------------
# TODO: compare with download_radolan()
download_and_extract_hourly_radolan_historical_bin <- function(year)
{
  stopifnot(is.integer(year))

  # If year is a vector of years, call this function for each year
  if (length(year) > 1L) {
    return(unlist(lapply(
      year,
      download_and_extract_hourly_radolan_historical_bin
    )))
  }

  stopifnot(length(year) == 1L)

  urls <- "grids_germany/hourly/radolan/historical/bin" %>%
    file.path(as.character(year)) %>%
    ftp_path_cdc() %>%
    list_url(recursive = TRUE, full_names = TRUE)

  # Download and extract the files
  unlist(lapply(
    X = urls,
    FUN = download_and_extract_hourly_radolan_historical_bin_url,
    target_dir = download_dir("dwd")
  ))
}

# download_and_extract_hourly_radolan_historical_bin_url -----------------------
# TODO: compare with download_radolan()
download_and_extract_hourly_radolan_historical_bin_url <- function(
    url, target_dir
)
{
  stopifnot(length(url) == 1L)
  stopifnot(grepl("hourly/radolan/historical/bin", url))
  stopifnot(endsWith(url, ".tar.gz"))

  file <- download_into_folder_structure(
    url,
    target_dir = target_dir,
    skip_url_segments = 3L,
    mode = "wb"
  )

  # Extract the binary files and return their paths
  unzip_dir <- get_relative_path(file) %>%
    temp_dir() %>%
    remove_right(nchar(".tar.gz")) %>%
    kwb.utils::createDirectory(dbg = FALSE)

  unzip_tar_gz_file(file, target_dir = unzip_dir)
}
