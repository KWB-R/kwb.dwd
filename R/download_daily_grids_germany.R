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
