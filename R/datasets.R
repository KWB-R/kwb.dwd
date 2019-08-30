#' Files Available on DWD's FTP Server
#'
#' Information on files that are available for download at
#' \url{ftp://opendata.dwd.de/climate_environment/CDC/}. The information were
#' last updated on 2019-06-22.
#'
#' @format A data frame with 438548 observations of 5 variables:
#' \describe{
#'   \item{url}{URL to the file on the FTP server}
#'   \item{type}{either "file" or "directory"}
#'   \item{size}{file size in bytes}
#'   \item{permissions}{string indicating file permissions}
#'   \item{modification_time}{time of last modification of the file}
#' }
#' @source \url{ftp://opendata.dwd.de/climate_environment/CDC/Announce_log_CDC_ftp.txt}
"dwd_files"
