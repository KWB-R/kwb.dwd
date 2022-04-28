# get_dwd_urls_metadata --------------------------------------------------------

#' Get URLs to DWD Metadata
#'
#' @return list of character with each element representing a URL to a metadata
#'   file provided by Deutscher Wetterdienst (DWT), e.g. files
#'   "_Beschreibung_Stationen.txt" describing measurement stations.
#'
#' @export
#' @importFrom stats setNames
#' @importFrom kwb.utils pasteColumns
get_dwd_urls_metadata <- function()
{
  # Note: category "solar" is not considered

  combinations <- get_dwd_config_combinations()

  directory_urls <- apply(combinations, 1, function(x) do.call(
    dwd_url_climate_dir, as.list(x)
  ))

  filenames <- apply(combinations[, 1:2], 1, function(x) do.call(
    dwd_filename_stations, as.list(x)
  ))

  urls <- paste0(directory_urls, "/", filenames)

  stats::setNames(urls, kwb.utils::pasteColumns(combinations, sep = "_"))
}

# get_dwd_config_combinations --------------------------------------------------

#' Get DWD config combinations
#'
#' @param frequencies default: c("daily", "hourly")
#' @param currentnesses default: c("historical", "recent")
#' @return data.frame with columns frequency category and currentness
#' @keywords internal
#' @noRd
#' @noMd
#' @importFrom  kwb.utils expandGrid
get_dwd_config_combinations <- function(
  frequencies = c("daily", "hourly"), currentnesses = c("historical", "recent")
)
{
  specification <- get_dwd_url_specification()

  do.call(rbind, lapply(frequencies, function(frequency) {

    available <- specification[, paste0("folder_", frequency)] != ""

    kwb.utils::expandGrid(
      frequency = frequency,
      category = rownames(specification)[available],
      currentness = currentnesses
    )
  }))
}

# get_dwd_url_specification ----------------------------------------------------
get_dwd_url_specification <- function(category = NULL)
{
  content <- paste(
    sep = ",",
    "category,file_prefix,folder_daily,folder_hourly",
    "air_temperature,TU,,air_temperature",
    "cloud_type,CS,,cloud_type",
    "cloudiness,N,,cloudiness",
    "climate,KL,kl,",
    "precipitation,RR,more_precip,precipitation",
    "pressure,P0,,pressure",
    "soil_temperature,EB,soil_temperature,soil_temperature",
    "sun,SD,,sun",
    "visibility,VV,,visibility",
    "snow,Wa,water_equiv,",
    "wind,FF,,wind"
  )

  result <- matrix(strsplit(content, ",")[[1]], ncol = 4, byrow = TRUE)

  dimnames(result) <- list(result[, 1], result[1, ])

  result <- result[-1, -1]

  if (is.null(category)) {
    return(result)
  }

  result[safe_element(category, rownames(result)), ]
}

# dwd_url_climate_dir ----------------------------------------------------------
dwd_url_climate_dir <- function(
  frequency = NULL, category = NULL, currentness = NULL
)
{
  url <- ftp_path_cdc("observations_germany/climate")

  if (is.null(frequency)) {
    return(url)
  }

  add_subdir <- function(a, b) paste0(a, "/", b)

  url <- add_subdir(url, safe_element(frequency, c("daily", "hourly")))

  if (is.null(category)) {
    return(url)
  }

  specification <- get_dwd_url_specification(category)

  subdir <- get_element_or_stop(specification, paste0("folder_", frequency))

  url <- add_subdir(url, subdir)

  if (is.null(currentness)) {
    return(url)
  }

  add_subdir(url, safe_element(currentness, c("historical", "recent")))
}

# ftp_path_cdc -----------------------------------------------------------------
ftp_path_cdc <- function(...)
{
  file.path("ftp://opendata.dwd.de/climate_environment/CDC", ...)
}

ftp_path_monthly_grids <- function(...)
{
  ftp_path_cdc("grids_germany/monthly", ...)
}

# dwd_filename_stations --------------------------------------------------------
dwd_filename_stations <- function(category, frequency)
{
  sprintf(
    "%s_%swerte_Beschreibung_Stationen.txt",
    get_element_or_stop(get_dwd_url_specification(category), "file_prefix"),
    frequency_prefix(frequency)
  )
}

# frequency_prefix -------------------------------------------------------------
frequency_prefix <- function(frequency)
{
  get_element_or_stop(c(daily = "Tages", hourly = "Stunden"), frequency)
}
