# configure_radolan ------------------------------------------------------------
configure_radolan <- function(
    from = NULL,
    to = NULL,
    resolution,
    format,
    year = NULL,
    config = NULL
)
{
  if (!is.null(config)) {
    stopifnot(inherits(config, "radolan_configuration"))
    return(config)
  }

  safe_element(resolution, c("hourly", "daily"))
  safe_element(format, c("bin", "asc"))

  if (is.null(year)) {
    stopifnot(!is.null(from), !is.null(to))
  } else {
    stopifnot(is.integer(year), length(year) > 0L)
  }

  year_month <- function(year, month) sprintf("%4d%02d", year, month)

  from <- kwb.utils::defaultIfNULL(from, year_month(year[1L], 1L))
  to <- kwb.utils::defaultIfNULL(to, year_month(year[length(year)], 12L))

  structure(class = "radolan_configuration", list(
    method = "radolan",
    variable = "precipitation",
    from = from,
    to = to,
    resolution = resolution,
    format = format
  ))
}
