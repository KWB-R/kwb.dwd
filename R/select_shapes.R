# select_shapes ----------------------------------------------------------------

#' Interactively Configure Selection of SpatialPolygons
#'
#' @param shapes list of SpatialPolygonsDataFrame objects, as e.g. returned by
#'   \code{\link{get_shapes_of_germany}}
#' @return list with elements \code{index} (index of SpatialPolygonsDataFrame in
#'   \code{shapes} list), \code{variable} (column in selected
#'   SpatialPolygonsDataFrame), \code{pattern} pattern (e.g. the name of a city)
#'   to be matched against the values in the selected column of the
#'   selected SpatialPolygonsDataFrame. This list simply describes how to select
#'   a SpatialPolygon from a list of SpatialPolygonsDataFrames
select_shapes <- function(shapes)
{
  # Let the user select a SpatialPolygonsDataFrame (by name of .shp file)
  shape_names <- basename(names(shapes))
  selection <- utils::select.list(shape_names, title = "Select layer")
  shape_index <- which(selection == shape_names)

  # Let the user select a column from the selected SpatialPolygonsDataFrame
  variable <- select_variable(shapes[[shape_index]])

  # Let the user enter a pattern to be matched against the values in the column
  pattern <- readline("pattern: ")

  result <- list(
    index = shape_index,
    variable = variable,
    pattern = pattern
  )

  cat("Code to create this configuration:\n")
  cat("config <-", kwb.utils::objectToText(result))

  result
}

# select_variable --------------------------------------------------------------

#' Let User Select a Column from a SpatialPolygonsDataFrame
#'
#' @param shape object of class SpatialPolygonsDataFrame
#' @return name of selected column
select_variable <- function(shape)
{
  first_values <- kwb.utils::excludeNULL(
    lapply(stats::setNames(nm = names(shape)), function(v) {
      values <- unique(shape[[v]])
      values <- values[! is.na(values)]
      if (length(values)) {
        head(values, 3L)
      }
    })
  )

  choices <- sapply(names(first_values), function(name) {
    x <- first_values[[name]]
    sprintf("%s (%s)", name, paste(x, collapse = ", "))
  })

  selection <- utils::select.list(choices, title = "Select variable")

  names(selection)
}
