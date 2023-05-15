# download_and_extract_radolan -------------------------------------------------
# TODO: compare with download_radolan()
download_and_extract_radolan <- function(year, resolution, format, ...)
{
  #kwb.utils::assignPackageObjects("kwb.dwd")
  #resolution <- "hourly"
  #resolution <- "daily"
  #format <- "bin"

  stopifnot(is.integer(year))
  stopifnot(resolution %in% c("hourly", "daily"))
  stopifnot(format %in% c("bin", "asc"))

  # If year is a vector of years, call this function for each year
  if (length(year) > 1L) {
    return(unlist(lapply(
      year,
      download_and_extract_radolan,
      resolution = resolution,
      format = format,
      ...
    )))
  }

  stopifnot(length(year) == 1L)

  urls <- "grids_germany/%s/radolan/historical/%s/%d" %>%
    sprintf(resolution, format, year) %>%
    ftp_path_cdc() %>%
    list_url(recursive = TRUE, full_names = TRUE)

  # Download and extract the files
  files <- unlist(lapply(
    X = urls,
    FUN = download_and_extract_radolan_url,
    target_dir = download_dir("dwd"),
    ...
  ))

  files
}

# download_and_extract_radolan_url ---------------------------------------------
# TODO: compare with download_radolan()
download_and_extract_radolan_url <- function(
    url, target_dir = download_dir("dwd"), timeout = 120
)
{
  #kwb.utils::assignPackageObjects("kwb.dwd")
  #url <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/hourly/radolan/historical/bin/2009/RW200901.tar.gz"
  #url <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/radolan/historical/bin/2006/SF-200610.tar.gz"
  #url <- urls[1L]

  stopifnot(length(url) == 1L)

  info <- get_radolan_metadata(url)

  resolution <- info$resolution
  format <- info$format

  stopifnot(resolution %in% c("daily", "hourly"))
  stopifnot(format %in% c("asc", "bin"))

  file <- download_into_folder_structure(
    url,
    target_dir = target_dir,
    skip_url_segments = 3L,
    mode = "wb",
    timeout = timeout
  )

  full_extension <- get_full_extension(url)

  # Extract the binary files and return their paths
  unzip_dir <- get_relative_path(file) %>%
    remove_right(nchar(full_extension) + 1L) %>%
    temp_dir()

  if (full_extension == "tar.gz") {

    extracted_files <- unzip_tar_gz_file(file, target_dir = unzip_dir)

  } else if (full_extension == "tar") {

    tar_gz_files <- unzip_tar_file(file, target_dir = unzip_dir)

    extracted_files <- try(unlist(lapply(
      tar_gz_files,
      unzip_tar_gz_file,
      target_dir = dirname(tar_gz_files[1L])
    )))

    # On success, delete the .tar.gz files that have been extracted
    if (!kwb.utils::isTryError(extracted_files)) {
      unlink(tar_gz_files)
    }
  }

  extracted_files
}
