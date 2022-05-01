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
  result <- get_file_metadata_from_attributes(matrices)

  # Keep only grid cells within "mask"
  masked_values <- lapply(matrices, function(m) m * geo_mask)

  # Function to apply a statistical function to all vectors in berlin_values
  get_stats <- function(fun) sapply(masked_values, fun, na.rm = TRUE)

  result$mean <- get_stats(mean)
  result$sd <- get_stats(stats::sd)
  result$min <- get_stats(min)
  result$max <- get_stats(max)
  result$n_values <- sapply(masked_values, function(x) sum(! is.na(x)))

  result
}

# get_file_metadata_from_attributes --------------------------------------------
get_file_metadata_from_attributes <- function(x)
{
  as.data.frame(lapply(
    stats::setNames(nm = c("file", "year", "month")),
    function(name) sapply(x, kwb.utils::getAttribute, name)
  ))
}
