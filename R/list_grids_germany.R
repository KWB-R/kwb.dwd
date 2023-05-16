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

# list_grids_germany -----------------------------------------------------------

#' Get URLs to Zipped Files Containing Grid Data for Germany
#'
#' @param resolution one of "monthly", "daily"
#' @param variable variable for which to look for URLs. Must be one of
#'   \code{kwb.dwd::list_url(kwb.dwd:::ftp_path_grids_germany(resolution))}
#' @param from optional. First month to be considered, as "yyyymm" string
#' @param to optional. Last month to be considered, as "yyyymm" string
#' @param recursive whether to list files recursively. Default: \code{TRUE}
#'
list_grids_germany <- function(
    resolution, extension, variable, from = NULL, to = NULL, recursive = TRUE
)
{
  #resolution = "monthly"
  #extension = ".asc.gz"

  # Code to get the possible choices
  # base_url <- kwb.dwd:::ftp_path_grids_germany(resolution)
  # kwb.dwd:::url_subdirs_containing_files_with_extension(base_url, extension)

  # Make sure that the given variable name is a possible choice
  if (resolution == "monthly") {

    safe_element(variable, c(
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

  } else if (resolution == "daily") {

    safe_element(variable, c(
      "evapo_p",
      "evapo_r",
      "frost_depth",
      "soil_moist",
      "soil_temperature_5cm"
    ))
  }

  # Base URL to grids of Germany in requested temporal resolution
  base_url <- ftp_path_grids_germany(resolution, variable)

  # List data files
  relative_urls <- base_url %>%
    list_url(recursive = recursive) %>%
    filter_by_extension(extension) %>%
    filter_by_month_range(from, to)

  # Provide full URLs to zipped files
  file.path(base_url, relative_urls)
}
