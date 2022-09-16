# download_daily_grids_germany -------------------------------------------------
download_daily_grids_germany <- function(
    variable,
    from = to,
    to = last_month_as_yyyymm()
)
{
  variable %>%
    list_daily_grids_germany(from, to) %>%
    lapply(download_and_extract) %>%
    unlist()
}

# list_daily_grids_germany -----------------------------------------------------
list_daily_grids_germany <- function(
    variable,
    from = to,
    to = last_month_as_yyyymm()
)
{
  # Base URL to daily grids
  base_url <- ftp_path_cdc("grids_germany/daily")

  # Code to get the possible choices
  # urls <- list_url(base_url, full_names = TRUE)
  # first_tgz <- lapply(urls, function(url) {
  #   head(grep("tgz$", list_url(url), value = TRUE))
  # })
  # print(basename(urls[lengths(first_tgz) > 0L]))

  choices <- as.list(stats::setNames(nm = c(
    "evapo_p",
    "evapo_r",
    "frost_depth",
    "soil_moist",
    "soil_temperature_5cm"
  )))

  # Make sure that the given variable name is a possible choice
  variable <- kwb.utils::selectElements(choices, variable)

  "grids_germany/daily" %>%
    kwb.dwd:::ftp_path_cdc(variable) %>%
    list_url(full_names = TRUE) %>%
    filter_for_tgz_extension() %>%
    filter_by_month_range(from, to)
}

# download_and_extract ---------------------------------------------------------
download_and_extract <- function(url)
{
  # Create a dedicated temporary folder
  target_dir <- temp_dir(kwb.utils::removeExtension(basename(url)))

  # Download the file into the dedicated folder
  tgz_file <- file.path(target_dir, basename(url))
  tgz_file <- download_if_not_there(url, tgz_file)

  # Extract the file into the same folder
  archive::archive_extract(tgz_file, dir = target_dir)

  # List the extracted files
  grid_files <- dir(target_dir, "\\.asc", full.names = TRUE)
}
