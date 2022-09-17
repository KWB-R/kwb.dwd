download_monthly_grids_germany <- function(
    variable,
    from = to,
    to = last_month_as_yyyymm(),
    urls = NULL
)
{
  if (is.null(urls)) {
    urls <- list_monthly_grids_germany_asc_gz(variable, from, to)
  }

  unlist(lapply(urls, function(url) {
    #url <- urls[1L]
    download_if_not_there(url, file = file.path(
      temp_dir(template. = kwb.utils::removeExtension(url)),
      basename(url)
    ))
  }))
}
