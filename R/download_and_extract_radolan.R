# download_and_extract_radolan -------------------------------------------------
# TODO: compare with download_radolan()
download_and_extract_radolan <- function(
    from = NULL,
    to = NULL,
    resolution,
    format,
    year = NULL,
    config = NULL,
    ...
)
{
  #kwb.utils::assignPackageObjects("kwb.dwd")

  # Create a configuration from the arguments, if required
  config <- configure_radolan(
    from = from,
    to = to,
    resolution = resolution,
    format = format,
    year = year,
    config = config
  )

  # Now, take the argument values from the configuration
  as_configured <- kwb.utils::createAccessor(config)

  from <- as_configured("from")
  to <- as_configured("to")
  resolution <- as_configured("resolution")
  format <- as_configured("format")

  # Pattern to filter for files related to months between from and to
  pattern <- sprintf("[^0-9](%s)[.]tar", month_range_pattern(from, to))

  urls <- kwb.utils::catAndRun(
    "Listing available files and filtering for related months",
    expr = {
      "grids_germany/%s/radolan/historical/%s" %>%
        sprintf(resolution, format) %>%
        ftp_path_cdc() %>%
        list_url(recursive = TRUE, full_names = TRUE, dbg = FALSE) %>%
        grep(pattern = pattern, value = TRUE)
    }
  )

  # Download and extract the files
  files <- unlist(lapply(
    X = urls,
    FUN = download_and_extract_radolan_url,
    target_dir = download_dir("dwd"),
    ...
  ))

  structure(files, config = config)
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
