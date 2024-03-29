# get_example_grid_germany -----------------------------------------------------

#' Read an Example Raster File for Germany
#'
#' Read monthly potential evaporation of January 2022, just as an example.
#' @export
get_example_grid_germany <- function()
{
  path <- "evapo_p/grids_germany_monthly_evapo_p_202201.asc.gz"
  read_asc_gz_file(url = ftp_path_monthly_grids(path))
}
