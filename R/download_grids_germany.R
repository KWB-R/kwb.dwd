download_grids_germany <- function(resolution, variable, from, to, quiet = FALSE)
{
  if (resolution == "daily") {

    # Download and extract .tgz files from DWD server
    download_daily_grids_germany(variable, from, to, quiet = quiet)

  } else if (resolution == "monthly") {

    # Download files from DWD server
    download_monthly_grids_germany(variable, from, to, quiet = quiet)

  } else {

    clean_stop("resolution must be one of 'daily', 'monthly'.")
  }
}
