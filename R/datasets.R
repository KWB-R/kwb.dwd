#' Files Available on DWD's FTP Server (deprecated)
#'
#' URLs to files that are/were available for download at
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC}. The file list was last updated on
#' 2019-05-31. According to DWD, this server closes on 2019-06-01. Instead,
#' files can be downloaded from
#' \url{ftp://opendata.dwd.de/climate_environment/CDC/},
#' see \code{\link{opendata_files}}.
#'
#' @format A data frame with 355311 rows and 2 variables:
#' \describe{
#'   \item{path}{URL of the path to the file on the FTP server}
#'   \item{file}{file name}
#' }
#' @source \url{ftp://ftp-cdc.dwd.de/pub/CDC/Announce_log_CDC_ftp.txt}
"dwd_files"

#' Files Available on DWD's FTP Server
#'
#' URLs to files that are available for download at
#' \url{ftp://opendata.dwd.de/climate_environment/CDC/}. The file list was last
#' updated on 2019-06-01.
#'
#' @format A data frame with 436687 rows and 2 variables:
#' \describe{
#'   \item{path}{URL of the path to the file on the FTP server}
#'   \item{file}{file name}
#' }
#' @source \url{ftp://opendata.dwd.de/climate_environment/CDC/Announce_log_CDC_ftp.txt}
"opendata_files"
