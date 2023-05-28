# read_hourly_radolan_historical_bin_for_region --------------------------------
read_hourly_radolan_historical_bin_for_region <- function(
    from = NULL,
    to = NULL,
    shape = NULL,
    year = NULL,
    dbg = TRUE,
    blocksize = 24L,
    run_parallel = TRUE,
    ...
)
{
  #kwb.utils::assignPackageObjects("kwb.dwd")
  #kwb.utils::assignArgumentDefaults(kwb.dwd:::read_hourly_radolan_historical_bin_for_region)
  #year <- 2009L
  #`%>%` <- magrittr::`%>%`
  #shape <- kwb.dwd:::get_shape_of_german_region("berlin")

  if (!is.null(year)) {
    stopifnot(is.integer(year), length(year) == 1L)
  }

  bin_files <- list_extracted_radolan_files(
    from = kwb.utils::defaultIfNULL(from, paste0(year, "01")),
    to = kwb.utils::defaultIfNULL(to, paste0(year, "12")),
    resolution = "hourly",
    format = "bin"
  )

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
