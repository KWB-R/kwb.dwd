# remotes::install_github("kwb-r/kwb.dwd@dev")

paths <- kwb.utils::resolve(list(
  radolan_root = "<desktop>/tmp/flusshygiene/radolan",
  radolan_bin = "<radolan_root>/daily/data/historical_unzipped",
  desktop = kwb.utils::desktop()
))

filenames <- dir(paths$radolan_bin, full.names = TRUE)

bin_file <- filenames[1]

microbenchmark::microbenchmark(
  times = 2,
  kwb.dwd:::read_binary_radolan_file_raw(bin_file, version = 1),
  kwb.dwd:::read_binary_radolan_file_raw(bin_file, version = 2),
  kwb.dwd:::read_binary_radolan_file_raw(bin_file, version = 3),
  check = function(results) {
    identical(as.numeric(results[[2]]), results[[1]]) &&
      identical(results[[2]], structure(results[[3]], flags = NULL))
  }
)

microbenchmark::microbenchmark(
  times = 100,
  kwb.dwd:::read_binary_radolan_file_raw(bin_file, version = 2),
  kwb.dwd:::read_binary_radolan_file_raw(bin_file, version = 3),
  check = function(results) {
    identical(results[[1]], structure(results[[2]], flags = NULL))
  }
)

rbi <- kwb.dwd:::read_binary_radolan_file_raw(bin_file)

microbenchmark::microbenchmark(
  times = 300,
  kwb.dwd:::radolan_raw_to_raster(rbi, version = 1),
  kwb.dwd:::radolan_raw_to_raster(rbi, version = 2),
  kwb.dwd:::radolan_raw_to_raster(rbi, version = 3),
  check = kwb.utils::allAreIdentical
)
