# download_daily_grids_germany -------------------------------------------------
download_daily_grids_germany <- function(
    variable,
    from = to,
    to = last_month(),
    quiet = FALSE
)
{
  list_grids_germany("daily", ".tgz", variable, from, to) %>%
    lapply(download_and_extract, quiet = quiet) %>%
    unlist()
}

# download_and_extract ---------------------------------------------------------
#' @importFrom archive archive_extract
download_and_extract <- function(url, quiet = FALSE)
{
  # Create a dedicated temporary folder
  target_dir <- temp_dir(template = url)

  # Download the file into the dedicated folder
  file <- download(
    url,
    target_dir = target_dir,
    quiet = quiet,
    mode = "wb"
  )

  # Extract the file into the same folder
  archive::archive_extract(file, dir = target_dir)

  # List the extracted files
  dir(target_dir, "\\.asc", full.names = TRUE)
}
