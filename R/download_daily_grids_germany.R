# download_daily_grids_germany -------------------------------------------------
download_daily_grids_germany <- function(
    variable,
    from = to,
    to = last_month_as_yyyymm(),
    quiet = FALSE
)
{
  variable %>%
    list_daily_grids_germany_tgz(from, to) %>%
    lapply(download_and_extract, quiet = quiet) %>%
    unlist()
}

# list_daily_grids_germany_tgz -------------------------------------------------
list_daily_grids_germany_tgz <- function(variable, from = NULL, to = NULL)
{
  # Base URL to daily grids
  base_url <- ftp_path_daily_grids()

  # Code to get the possible choices
  # base_url <- kwb.dwd:::ftp_path_cdc("grids_germany/daily")
  # kwb.dwd:::url_subdirs_containing_files_with_extension(base_url, ".tgz")

  # Make sure that the given variable name is a possible choice
  variable <- match.arg(variable, c(
    "evapo_p",
    "evapo_r",
    "frost_depth",
    "soil_moist",
    "soil_temperature_5cm"
  ))

  "grids_germany/daily" %>%
    ftp_path_cdc(variable) %>%
    list_url(full_names = TRUE) %>%
    filter_by_extension_tgz() %>%
    filter_by_month_range(from, to)
}

# download_and_extract ---------------------------------------------------------
download_and_extract <- function(url, quiet = FALSE)
{
  # Create a dedicated temporary folder
  target_dir <- temp_dir(template. = url)

  # Download the file into the dedicated folder
  file <- download_if_not_there(
    url,
    file.path(target_dir, basename(url)),
    quiet = quiet
  )

  # Extract the file into the same folder
  archive::archive_extract(file, dir = target_dir)

  # List the extracted files
  dir(target_dir, "\\.asc", full.names = TRUE)
}
