#' Files Available on DWD's FTP Server
#'
#' Information on files that are available for download at
#' \url{ftp://opendata.dwd.de/climate_environment/CDC/} [base_url]. The
#' information were last updated on 2023-04-26.
#'
#' @format A data frame with 838345 observations of 8 variables:
#' \describe{
#'   \item{file}{path to the file below [base_url]}
#'   \item{isdir}{TRUE for a directory, FALSE for a file}
#'   \item{size}{file size in bytes}
#'   \item{permissions}{string indicating file permissions}
#'   \item{modification_time}{time of last modification of the file}
#'   \item{user}{further column provided by the FTP server}
#'   \item{group}{further column provided by the FTP server}
#'   \item{links}{further column provided by the FTP server}
#' }
#' @source \url{ftp://opendata.dwd.de/climate_environment/CDC/Announce_log_CDC_ftp.txt}
"dwd_files"
