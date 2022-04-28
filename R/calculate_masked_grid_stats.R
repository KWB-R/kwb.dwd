# calculate_masked_grid_stats --------------------------------------------------

#' Calculate Stats of Variable for Geographical Subset of a Grid
#'
#' @param matrices matrices
#' @param geo_mask "mask matrix" defining a geographical subset
#'
#' @return data frame with one row per matrix in \code{matrices} and columns
#'   \code{file}, \code{year}, \code{month}, \code{mean}, \code{sd}, \code{min},
#'   \code{max}
#' @export
#' @importFrom kwb.utils getAttribute
#' @importFrom stats sd
calculate_masked_grid_stats <- function(matrices, geo_mask)
{
  # Start with metadata from matrices' attributes: file name, year, month
  result <- as.data.frame(lapply(
    X = stats::setNames(nm = c("file", "year", "month")),
    FUN = function(x) sapply(matrices, kwb.utils::getAttribute, x)
  ))

  # Keep only grid cells within "mask"
  masked_values <- lapply(matrices, function(m) m * geo_mask)

  # Function to apply a statistical function to all vectors in berlin_values
  get_stats <- function(fun) sapply(masked_values, fun, na.rm = TRUE)

  result$mean <- get_stats(mean)
  result$sd <- get_stats(stats::sd)
  result$min <- get_stats(min)
  result$max <- get_stats(max)

  result
}
