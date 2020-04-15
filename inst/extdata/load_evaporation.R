#
# Load evaporation data from DWD server
#
# Source the whole script first to load the function defined below
# Manually go through the commands within the MAIN section
#

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  # Base URL to potential evaporation files on DWD server
  base_url <- kwb.dwd:::ftp_path_cdc("grids_germany/monthly/evapo_p")
  
  # List data files
  relative_urls <- grep(
    "\\.asc\\.gz$", kwb.dwd::list_url(base_url), value = TRUE
  )
  
  # Provide full paths
  urls <- file.path(base_url, relative_urls)

  # Read all files into a list of matrices  
  evaporation_matrices <- lapply(urls, read_evaporation_matrix_from_url)

  # Provide metadata: file name, year, month  
  file_info <- data.frame(
    file = relative_urls,
    year = sapply(evaporation_matrices, kwb.utils::getAttribute, "year"),
    month = sapply(evaporation_matrices, kwb.utils::getAttribute, "month")
  )
  
  head(file_info)
  
  str(evaporation_matrices[[1]])
  
  # Get Berlin matrix, same size as DWD evpo matrix (Berlin grid cells set to 1, rest of cells = NA)
    Berlin_DWD_mask <- Berlin_DWD_mask()
  
  # calculate monthly stats for Berlin
    Berlin_evap_monthly <- evaporation_stats(evaporation_matrices = evaporation_matrices,
                                             file_info = file_info,
                                             geo_mask = Berlin_DWD_mask)
  
}

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
    as.matrix(read.table(text = text[-(1:6)])),
    header = text[1:6],
    year = extract_date_part(1L),
    month = extract_date_part(2L)
  )
}

# get geographical "stamp" for Berlin area ---------------------------------
Berlin_DWD_mask <- function()
{
  #DWD matrix filled with NA
  DWD_dim <- matrix(NA, nrow = 866, ncol = 654)
  
  #get Berlin coordinates...I did not find out how to link to the file I uploaded!!!
  Berlin_coordinates <- foreign::read.dbf('C:/Aendu_lokal/ABIMO_Paper/Daten/Karten/Hilfsgrid_DWD/Grid_Berlin_DWD.dbf')[,6:7]
  
  #set Berlin cells to 1 
  Berlin_matrix <- DWD_dim
  
  for (i in 1:length(grid_Berlin_DWD$row)) {
      Berlin_matrix[grid_Berlin_DWD$row[i], grid_Berlin_DWD$col[i]] <- 1
  }
  
  Berlin_matrix
  
}

# calculate stats of potential evaporation for geographical subset -------------------------------------------
evaporation_stats <- function(evaporation_matrices,
                              file_info,
                              geo_mask)
{
  pot_evap_stat <- file_info
  

  for (i in 1:length(evaporation_matrices)) {
    
    #keep only Berlin grid cells  
    Berlin_values <- evaporation_matrices[[i]] * geo_mask
    
    #correct unit to mm
    Berlin_values <- Berlin_values / 10
    
    pot_evap_stat$mean[i] <- mean(Berlin_values, na.rm = TRUE)
    pot_evap_stat$sd[i] <- sd(Berlin_values, na.rm = TRUE)
    pot_evap_stat$min[i] <- min(Berlin_values, na.rm = TRUE)
    pot_evap_stat$max[i] <- max(Berlin_values, na.rm = TRUE)
    
  }
  
  pot_evap_stat
  
}

