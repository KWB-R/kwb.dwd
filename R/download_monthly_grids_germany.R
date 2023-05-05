#kwb.utils::assignPackageObjects("kwb.dwd")

if (FALSE)
{
  kwb.dwd:::download_monthly_grids_germany(
    variable = "air_temperature_mean"
  )
}

download_monthly_grids_germany <- function(
    variable,
    from = to,
    to = last_month_as_yyyymm(),
    urls = NULL,
    quiet = FALSE
)
{
  if (is.null(urls)) {
    urls <- list_monthly_grids_germany_asc_gz(variable, from, to)
  }

  download_into_folder_structure(
    urls,
    target_dir = download_dir("dwd"),
    skip_url_segments = 4L,
    mode = "wb"
  )
}
