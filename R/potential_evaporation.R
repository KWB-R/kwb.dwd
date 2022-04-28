# get geographical "stamp" for Berlin area -------------------------------------
#' Get geographical "stamp" for Berlin area
#'
#' @return ???
#' @export
#' @importFrom utils read.csv
#' @examples
#' get_berlin_dwd_mask()
get_berlin_dwd_mask <- function()
{
  #DWD matrix filled with NA
  berlin_matrix <- matrix(NA, nrow = 866, ncol = 654)

  #get Berlin coordinates
  berlin_coordinates <- utils::read.csv(system.file(
    "extdata/berlin_coordinates.csv", package = "kwb.dwd"
  ))

  #set Berlin cells to 1

  for (i in seq_along(berlin_coordinates$row)) {
    berlin_matrix[berlin_coordinates$row[i], berlin_coordinates$col[i]] <- 1
  }

  berlin_matrix
}

# calculate_potential_evaporation_stats ----------------------------------------

#' Calculate stats of potential evaporation for geographical subset
#'
#' @param matrices matrices
#' @param geo_mask geo_mask
#'
#' @return ???
#' @export
#' @importFrom kwb.utils getAttribute
#' @importFrom stats sd
calculate_potential_evaporation_stats <- function(matrices, geo_mask)
{
  # Keep only Berlin grid cells and correct unit to mm
  berlin_values <- lapply(matrices, function(m) m * geo_mask / 10)

  # Start with metadata from matrices' attributes: file name, year, month
  pot_evap_stat <- as.data.frame(lapply(
    X = stats::setNames(nm = c("file", "year", "month")),
    FUN = function(x) sapply(matrices, kwb.utils::getAttribute, x)
  ))

  # Function to apply a statistical function to all vectors in berlin_values
  get_stats <- function(fun) sapply(berlin_values, fun, na.rm = TRUE)

  pot_evap_stat$mean <- get_stats(mean)
  pot_evap_stat$sd <- get_stats(stats::sd)
  pot_evap_stat$min <- get_stats(min)
  pot_evap_stat$max <- get_stats(max)

  pot_evap_stat
}
