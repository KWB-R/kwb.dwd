# read_evaporation_matrix_from_url ---------------------------------------------
read_evaporation_matrix_from_url <- function(url)
{
  stopifnot(is.character(url), length(url) == 1L)

  file_name <- basename(url)

  file <- file.path(tempdir(), file_name)

  download.file(url, file)

  con <- gzfile(file)

  on.exit(close(con))

  text <- readLines(con)

  year_month <- kwb.utils::extractSubstring("(\\d{4})(\\d{2})", file_name, 1:2)

  extract_date_part <- function(i) as.integer(year_month[[i]])

  structure(
    as.matrix(utils::read.table(text = text[-(1:6)])),
    header = text[1:6],
    year = extract_date_part(1L),
    month = extract_date_part(2L)
  )
}

# get geographical "stamp" for Berlin area -------------------------------------
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

# calculate stats of potential evaporation for geographical subset -------------
evaporation_stats <- function(evaporation_matrices, file_info, geo_mask)
{
  pot_evap_stat <- file_info

  for (i in seq_along(evaporation_matrices)) {

    #keep only Berlin grid cells
    berlin_values <- evaporation_matrices[[i]] * geo_mask

    #correct unit to mm
    berlin_values <- berlin_values / 10

    get_stats <- function(fun) fun(berlin_values, na.rm = TRUE)
    pot_evap_stat$mean[i] <- get_stats(mean)
    pot_evap_stat$sd[i] <- get_stats(stats::sd)
    pot_evap_stat$min[i] <- get_stats(min)
    pot_evap_stat$max[i] <- get_stats(max)
  }

  pot_evap_stat
}
