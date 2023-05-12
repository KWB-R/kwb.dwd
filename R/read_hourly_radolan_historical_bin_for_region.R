# read_hourly_radolan_historical_bin_for_region --------------------------------
read_hourly_radolan_historical_bin_for_region <- function(
    year, shape, dbg = TRUE, blocksize = 24L, run_parallel = TRUE,
    pattern = NULL, ...
)
{
  #kwb.utils::assignPackageObjects("kwb.dwd")
  #kwb.utils::assignArgumentDefaults(kwb.dwd:::read_hourly_radolan_historical_bin_for_region)
  #year <- 2009L
  #`%>%` <- magrittr::`%>%`
  #shape <- kwb.dwd:::get_shape_of_german_region("berlin")

  stopifnot(is.integer(year), length(year) == 1L)

  # files may optionally be zipped
  pattern <- kwb.utils::defaultIfNULL(pattern, "--bin(\\.gz)?$")

  # List locally available extracted binary files
  bin_files <- "grids_germany/hourly/radolan/historical/bin/" %>%
    paste0(year) %>%
    temp_dir() %>%
    dir(pattern = pattern, recursive = TRUE, full.names = TRUE)

  if (length(bin_files) == 0L) {
    writeLines(c(
      "No binary files locally available. ",
      "Please run download_and_extract_radolan() first."
    ))
    return(character())
  }

  #file_sizes_mib <- file.size(bin_files)/2^20
  #max(which(cumsum(file_sizes_mib) <= 1024))
  #sum(file_sizes_mib[1:663])

  # Read data from binary files
  kwb.utils::catAndRun(
    sprintf("Reading data from %d binary files", length(bin_files)),
    dbg = dbg,
    get_regional_stats_from_radolan_bin_files(
      bin_files,
      shape = shape,
      blocksize = blocksize,
      run_parallel = run_parallel
      , ...
    )
  )
}

# get_regional_stats_from_radolan_bin_files ------------------------------------
get_regional_stats_from_radolan_bin_files <- function(bin_files, shape, ...)
{
  get_regional_stats_from_radolan_files(
    bin_files,
    shape = shape,
    read_function = read_binary_radolan_file,
    consider_flags = TRUE,
    ...
  )
}
