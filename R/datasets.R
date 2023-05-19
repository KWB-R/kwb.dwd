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

#' Shapes of German Regions (0)
#'
#' Object of class "sf" representing a "Simple feature collection".
#'
#' @format Simple feature collection with 1 feature and 2 fields:
#'   \code{COUNTRY}, \code{ID_0}
#' @source
#' \url{https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip}
"shapes_germany_0"

#' Shapes of German Regions (1)
#'
#' Object of class "sf" representing a "Simple feature collection".
#'
#' @format Simple feature collection with 16 features and 11 fields:
#'   \code{ID_0}, \code{COUNTRY}, \code{ID_1}, \code{NAME_1}, \code{VARNAME_1},
#'   \code{NL_NAME_1}, \code{TYPE_1}, \code{ENGTYPE_1}, \code{CC_1},
#'   \code{HASC_1}, \code{ISO_1}
#' @source
#' \url{https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip}
"shapes_germany_1"

#' Shapes of German Regions (2)
#'
#' Object of class "sf" representing a "Simple feature collection".
#'
#' @format Simple feature collection with 403 features and 12 fields:
#'   \code{ID_0}, \code{COUNTRY}, \code{NAME_1}, \code{NL_NAME_1}, \code{ID_2},
#'   \code{NAME_2}, \code{VARNAME_2}, \code{NL_NAME_2}, \code{TYPE_2},
#'   \code{ENGTYPE_2}, \code{CC_2}, \code{HASC_2}
#' @source
#' \url{https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip}
"shapes_germany_2"
