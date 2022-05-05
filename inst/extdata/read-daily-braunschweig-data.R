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
