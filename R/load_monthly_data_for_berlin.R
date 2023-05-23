#' Load monthly potential evaporation for Berlin from DWD
#'
#' This function loads monthly potential evaporation for Berlin, Germany,
#' from Deutscher Wetterdienst (DWD).
#'
#' @param from optional. First month to be considered, as yyyymm string
#' @param to optional. Last month to be considered, as yyyymm string
#' @return data frame with columns
#'   * `file` (name of file downloaded from DWD),
#'   * `year` (year number as integer),
#'   * `month number as integer`,
#'   * `mean` (mean value),
#'   * `sd` (standard deviation),
#'   * `min` (minimum value),
#'   * `max` (maximum value),
#'   * `n_values` (number of considered values)
#' of potential evaporation calculated for Berlin, Germany
#' @export
load_potential_evaporation_berlin <- function(from = NULL, to = NULL)
{
  load_monthly_variable_for_region(
    variable = "evapo_p", region = "berlin", scale = 0.1, from, to
  )
}

#' Load monthly precipitation for Berlin from DWD
#'
#' This function loads monthly precipitation for Berlin, Germany,
#' from Deutscher Wetterdienst (DWD).
#'
#' @param from optional. First month to be considered, as yyyymm string
#' @param to optional. Last month to be considered, as yyyymm string
#' @return data frame with columns
#'   * `file` (name of file downloaded from DWD),
#'   * `year` (year number as integer),
#'   * `month number as integer`,
#'   * `mean` (mean value),
#'   * `sd` (standard deviation),
#'   * `min` (minimum value),
#'   * `max` (maximum value)
#' of precipitation calculated for Berlin, Germany
#' @export
load_precipitation_berlin <- function(from = NULL, to = NULL)
{
  load_monthly_variable_for_region(
    "precipitation", "berlin", scale = NULL, from, to
  )
}

# load_potential_evaporation_berlin_2 ------------------------------------------

#' New Version of Reading Potential Evaporation for Berlin
#'
#' This version uses publicly available shape files to determine the area that
#' is covered by Berlin. This function can easily be adopted to other cities!
#'
#' @param from optional. First month to be considered, as yyyymm string
#' @param to optional. Last month to be considered, as yyyymm string
#' @return data frame with columns
#'   * `file` (name of file downloaded from DWD),
#'   * `year` (year number as integer),
#'   * `month` (month number as integer`,
#'   * `mean` (mean value),
#'   * `sd` (standard deviation),
#'   * `min` (minimum value),
#'   * `max` (maximum value),
#'   * `n_values` (number of considered values)
#' of potential evaporation calculated for Berlin, Germany
#' @export
load_potential_evaporation_berlin_2 <- function(from, to)
{
  load_monthly_variable_for_region(
    "evapo_p", "berlin", scale = 0.1, from, to, version = 2L
  )
}
