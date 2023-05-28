#' Files Available on DWD's FTP Server
#'
#' Information on files that are available for download at
#' <ftp://opendata.dwd.de/climate_environment/CDC/> \[base_url\]. The
#' information were last updated on 2023-04-26.
#'
#' @format A data frame with 838345 observations of 8 variables:
#' \describe{
#'   \item{file}{path to the file below \[base_ur\]}
#'   \item{isdir}{TRUE for a directory, FALSE for a file}
#'   \item{size}{file size in bytes}
#'   \item{permissions}{string indicating file permissions}
#'   \item{modification_time}{time of last modification of the file}
#'   \item{user}{further column provided by the FTP server}
#'   \item{group}{further column provided by the FTP server}
#'   \item{links}{further column provided by the FTP server}
#' }
#' @source <ftp://opendata.dwd.de/climate_environment/CDC/Announce_log_CDC_ftp.txt>
"dwd_files"

#' Shapes of German Regions (0)
#'
#' Object of class "sf" representing a "Simple feature collection".
#'
#' @format Simple feature collection with 1 feature and 2 fields:
#'   * `COUNTRY`,
#'   * `ID_0`.
#' @source
#' <https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip>
"shapes_germany_0"

#' Shapes of German Regions (1)
#'
#' Object of class "sf" representing a "Simple feature collection".
#'
#' @format Simple feature collection with 16 features and 11 fields:
#'   * `ID_0`,
#'   * `COUNTRY`,
#'   * `ID_1`,
#'   * `NAME_1`,
#'   * `VARNAME_1`,
#'   * `NL_NAME_1`,
#'   * `TYPE_1`,
#'   * `ENGTYPE_1`,
#'   * `CC_1`,
#'   * `HASC_1`,
#'   * `ISO_1`.
#' @source
#' <https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip>
"shapes_germany_1"

#' Shapes of German Regions (2)
#'
#' Object of class "sf" representing a "Simple feature collection".
#'
#' @format Simple feature collection with 403 features and 12 fields:
#' * `ID_0`,
#' * `COUNTRY`,
#' * `NAME_1`,
#' * `NL_NAME_1`,
#' * `ID_2`,
#' * `NAME_2`,
#' * `VARNAME_2`,
#' * `NL_NAME_2`,
#' * `TYPE_2`,
#' * `ENGTYPE_2`,
#' * `CC_2`,
#' * `HASC_2`.
#' @source
#' <https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip>
"shapes_germany_2"
