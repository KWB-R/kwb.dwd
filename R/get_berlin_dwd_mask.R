# get_berlin_dwd_mask ----------------------------------------------------------

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
