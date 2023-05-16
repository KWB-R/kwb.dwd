if (FALSE)
{
  kwb.utils::assignPackageObjects("kwb.dwd")

  # Code to get the possible choices
  get_variable_choices <- function(frequency, extension)
  {
    stopifnot(frequency %in% c("daily", "monthly", "hourly"))
    url_subdirs_containing_files_with_extension(
      url = ftp_path_cdc("grids_germany", frequency),
      extension = extension
    )
  }

  open_radolan_description <- function(path)
  {
    file.path("grids_germany/hourly/radolan", path) %>%
      ftp_path_cdc() %>%
      list_url(full_names = TRUE) %>%
      `[`(1L) %>%
      open_description()
  }

  open_radolan_description(path = "historical/bin")
  # Historische stuendliche RADOLAN-Raster der Niederschlagshoehe (binaer)

  open_radolan_description(path = "historical/asc")
  # Historische stuendliche RADOLAN-Raster der Niederschlagshoehe (GIS-lesbar)

  # get_variable_choices("hourly", ".tar") # -> "radolan"
  # get_variable_choices("hourly", ".tar.gz") # -> "radolan"

  variables_grids_germany <- list(
    # get_variable_choices("daily", ".tgz")
    daily_tgz = c(
      "evapo_p",
      "evapo_r",
      "frost_depth",
      "soil_moist",
      "soil_temperature_5cm"
    ),
    # get_variable_choices("monthly", ".asc.gz")
    monthly_asc_gz <- c(
      "air_temperature_max",
      "air_temperature_mean",
      "air_temperature_min",
      "drought_index",
      "evapo_p",
      "evapo_r",
      "frost_depth",
      "precipitation",
      "soil_moist",
      "soil_temperature_5cm",
      "sunshine_duration"
    )
  )
}

# list_daily_grids_germany_tgz -------------------------------------------------
list_daily_grids_germany_tgz <- function(
    variable,
    from = NULL,
    to = NULL
)
{
  # Base URL to daily grids
  base_url <- ftp_path_grids_germany("daily")

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
    filter_by_extension(".tgz") %>%
    filter_by_month_range(from, to)
}

# list_monthly_grids_germany_asc_gz --------------------------------------------

#' Get URLs to Monthly Grids in Zipped ESRI-ascii-grid Format
#'
#' @param variable variable for which to look for URLs. Must be one of
#'   \code{kwb.dwd::list_url(kwb.dwd:::ftp_path_grids_germany("monthly"))}
#' @param from optional. First month to be considered, as "yyyymm" string
#' @param to optional. Last month to be considered, as "yyyymm" string
#' @param recursive whether to list files recursively. Default: \code{TRUE}
list_monthly_grids_germany_asc_gz <- function(
    variable,
    from = NULL,
    to = NULL,
    recursive = TRUE
)
{
  base_url <- ftp_path_grids_germany("monthly", variable)

  # Code to get the possible choices
  # base_url <- kwb.dwd:::ftp_path_grids_germany("monthly")
  # kwb.dwd:::url_subdirs_containing_files_with_extension(base_url, ".asc.gz")

  # Make sure that the given variable name is a possible choice
  variable <- match.arg(variable, c(
    "air_temperature_max",
    "air_temperature_mean",
    "air_temperature_min",
    "drought_index",
    "evapo_p",
    "evapo_r",
    "frost_depth",
    "precipitation",
    "soil_moist",
    "soil_temperature_5cm",
    "sunshine_duration"
  ))

  # List data files
  relative_urls <- base_url %>%
    list_url(recursive = recursive) %>%
    filter_by_extension(".asc.gz") %>%
    filter_by_month_range(from, to)

  # Provide full paths to zipped files in ESRI-ascii-grid-format
  file.path(base_url, relative_urls)
}
