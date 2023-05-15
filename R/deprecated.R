# download_and_extract_hourly_radolan_historical_bin ---------------------------
# TODO: compare with download_radolan()
download_and_extract_hourly_radolan_historical_bin <- function(year)
{
  kwb.utils::warningDeprecated(
    old_name = "download_and_extract_hourly_radolan_historical_bin()",
    new_name = sprintf(
      "download_and_extract_radolan(resolution = '%s', format = '%s')",
      "hourly", "bin"
    ),
    parentheses = FALSE
  )

  download_and_extract_radolan(
    year = year,
    resolution = "hourly",
    format = "bin"
  )
}

# download_and_extract_hourly_radolan_historical_asc ---------------------------
download_and_extract_hourly_radolan_historical_asc <- function(
    url, target_dir = download_dir("dwd")
)
{
  stopifnot(length(url) == 1L)
  stopifnot(grepl("hourly/radolan/historical/asc", url))
  stopifnot(endsWith(url, ".tar"))

  file <- download_into_folder_structure(
    url,
    target_dir = target_dir,
    skip_url_segments = 4L,
    mode = "wb"
  )

  unzip_dir <- temp_dir(
    "grids_germany",
    dirname(get_relative_path(file, target_dir)),
    remove_right(basename(file), nchar(".tar"))
  )

  tar_gz_files <- unzip_tar_file(file = file, target_dir = unzip_dir)

  result <- try(unlist(lapply(
    tar_gz_files,
    unzip_tar_gz_file,
    target_dir = dirname(tar_gz_files[1L])
  )))

  # On success, delete the .tar.gz files that have been extracted
  if (!kwb.utils::isTryError(result)) {
    unlink(tar_gz_files)
  }

  result
}
