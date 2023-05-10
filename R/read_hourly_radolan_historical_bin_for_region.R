# read_hourly_radolan_historical_bin_for_region --------------------------------
read_hourly_radolan_historical_bin_for_region <- function(
    year, shape, dbg = TRUE, blocksize = 24L, run_parallel = TRUE
)
{
  stopifnot(is.integer(year), length(year) == 1L)

  # List locally available extracted binary files
  bin_files <- "grids_germany/hourly/radolan/historical/bin/" %>%
    paste0(year) %>%
    temp_dir() %>%
    dir(pattern = "--bin$", recursive = TRUE, full.names = TRUE)

  if (length(bin_files) == 0L) {
    writeLines(c(
      "No binary files locally available. ",
      "Please run download_and_extract_hourly_radolan_historical_bin() first."
    ))
    return(character())
  }

  # Read data from binary files
  kwb.utils::catAndRun(
    sprintf("Reading data from %d binary files", length(bin_files)),
    dbg = dbg,
    get_regional_stats_from_radolan_bin_files(
      bin_files,
      shape = shape,
      blocksize = blocksize,
      run_parallel = run_parallel
    )
  )
}

# get_regional_stats_from_radolan_bin_files ------------------------------------
get_regional_stats_from_radolan_bin_files <- function(
    bin_files, shape, blocksize = 24L, dbg = TRUE, run_parallel = TRUE
)
{
  get_regional_stats_from_radolan_files(
    bin_files,
    shape = shape,
    read_function = read_binary_radolan_file,
    consider_flags = TRUE,
    blocksize = blocksize,
    dbg = dbg,
    run_parallel = run_parallel
  )
}
