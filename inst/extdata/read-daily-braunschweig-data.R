#
# List URLs with monthly/daily grid data for Germany
#
if (FALSE)
{
  # monthly

  urls_monthly <- kwb.dwd:::list_monthly_grids_germany_asc_gz("x")
  # -> error with possible variables

  urls_monthly <- kwb.dwd:::list_monthly_grids_germany_asc_gz("sunshine_duration")

  # daily
  urls_daily <- kwb.dwd:::list_daily_grids_germany_tgz("x")
  # -> error with possible variables

  urls_daily <- kwb.dwd:::list_daily_grids_germany_tgz("soil_temperature_5cm")

  kwb.file::remove_common_root(urls_monthly)
  kwb.file::remove_common_root(urls_daily)
}

#
# Download daily grids for Germany (.asc files)
#
if (FALSE)
{
  from = "202201"
  to = "202207"

  files_evapo_p <- kwb.dwd:::download_daily_grids_germany("evapo_p", from, to)
  files_evapo_r <- kwb.dwd:::download_daily_grids_germany("evapo_r", from, to)
}

#
# Read daily data from DWD, mask region with given shape file
#

if (FALSE)
{
  shape_file <- "~/../Downloads/A/Abwasserverregnungsgebiet/Abwasserverregnungsgebiet.shp"

  # Only data of full months can currently be read!
  evapo_p <- kwb.dwd::read_daily_data_over_shape(
    file = shape_file,
    variable = "evapo_p",
    from = "202001",
    to = "202002"
  )

  View(evapo_p)
}
