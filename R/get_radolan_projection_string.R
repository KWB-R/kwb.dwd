#' Compose radolan projection string
#'
#' @return default radolan projection string
#' @export
#'
#' @examples
#' get_radolan_projection_string()
#'
get_radolan_projection_string <- function()
{
  to_projection_string(
    parameters = list(
      proj = "stere",
      lat_0 = 90,
      lat_ts = 90,
      lon_0 = 10,
      k = 0.93301270189,
      x_0 = 0,
      y_0 = 0,
      a = 6370040,
      b = 6370040,
      to_meter = 1000
    ),
    switches = "no_defs"
  )
}

# to_projection_string ---------------------------------------------------------
to_projection_string <- function(parameters, switches)
{
  assignments <- paste0(names(parameters), "=", as.character(parameters))
  paste0("+", c(assignments, switches), collapse = " ")
}
