# ftp_path_cdc -----------------------------------------------------------------
ftp_path_cdc <- function(...)
{
  file.path("ftp://opendata.dwd.de/climate_environment/CDC", ...)
}

# ftp_path_monthly_grids -------------------------------------------------------
ftp_path_monthly_grids <- function(...)
{
  ftp_path_cdc("grids_germany/monthly", ...)
}

# ftp_path_daily_grids ---------------------------------------------------------
ftp_path_daily_grids <- function(...)
{
  ftp_path_cdc("grids_germany/daily", ...)
}
