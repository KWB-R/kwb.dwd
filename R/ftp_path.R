# ftp_path_cdc -----------------------------------------------------------------
ftp_path_cdc <- function(...)
{
  file.path("ftp://opendata.dwd.de/climate_environment/CDC", ...)
}

# ftp_path_grids_germany -------------------------------------------------------
ftp_path_grids_germany <- function(resolution, ...)
{
  safe_element(resolution, c("monthly", "daily"))

  ftp_path_cdc("grids_germany", resolution, ...)
}
