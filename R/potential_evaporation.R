# read_potential_evaporation_from_url ------------------------------------------
#' Read potential evaporation from URL
#'
#' @param url url
#' @return ???
#' @keywords internal
#' @noMd
#' @noRd
#' @importFrom kwb.utils extractSubstring
#' @importFrom utils read.table
read_potential_evaporation_from_url <- function(url)
{
  text <- read_lines_from_downloaded_gz(url)

  file_name <- basename(url)

  year_month <- kwb.utils::extractSubstring("(\\d{4})(\\d{2})", file_name, 1:2)

  extract_date_part <- function(i) as.integer(year_month[[i]])

  structure(
    as.matrix(utils::read.table(text = text[-(1:6)])),
    header = text[1:6],
    year = extract_date_part(1L),
    month = extract_date_part(2L),
    file = file_name,
    origin = dirname(url)
  )
}

# read_lines_from_downloaded_gz ------------------------------------------------
read_lines_from_downloaded_gz <- function(url)
{
  stopifnot(is.character(url), length(url) == 1L)

  file <- file.path(tempdir(), basename(url))

  download.file(url, file, method = "auto")

  con <- gzfile(file)

  on.exit(close(con))

  readLines(con)
}

# get geographical "stamp" for Berlin area -------------------------------------
#' Get geographical "stamp" for Berlin area
#'
#' @return ???
#' @export
#' @importFrom utils read.csv
#' @examples
#' get_berlin_dwd_mask()
get_berlin_dwd_mask <- function()
{
  #DWD matrix filled with NA
  berlin_matrix <- matrix(NA, nrow = 866, ncol = 654)

  #get Berlin coordinates
  berlin_coordinates <- utils::read.csv(system.file(
    "extdata/berlin_coordinates.csv", package = "kwb.dwd"
  ))

  #set Berlin cells to 1

  for (i in seq_along(berlin_coordinates$row)) {
    berlin_matrix[berlin_coordinates$row[i], berlin_coordinates$col[i]] <- 1
  }

  berlin_matrix
}

# calculate_potential_evaporation_stats ----------------------------------------

#' Calculate stats of potential evaporation for geographical subset
#'
#' @param matrices matrices
#' @param geo_mask geo_mask
#'
#' @return ???
#' @export
#' @importFrom kwb.utils getAttribute
#' @importFrom stats sd
calculate_potential_evaporation_stats <- function(matrices, geo_mask)
{
  # Keep only Berlin grid cells and correct unit to mm
  berlin_values <- lapply(matrices, function(m) m * geo_mask / 10)

  # Start with metadata from matrices' attributes: file name, year, month
  pot_evap_stat <- as.data.frame(lapply(
    X = stats::setNames(nm = c("file", "year", "month")),
    FUN = function(x) sapply(matrices, kwb.utils::getAttribute, x)
  ))

  # Function to apply a statistical function to all vectors in berlin_values
  get_stats <- function(fun) sapply(berlin_values, fun, na.rm = TRUE)

  pot_evap_stat$mean <- get_stats(mean)
  pot_evap_stat$sd <- get_stats(stats::sd)
  pot_evap_stat$min <- get_stats(min)
  pot_evap_stat$max <- get_stats(max)

  pot_evap_stat
}
