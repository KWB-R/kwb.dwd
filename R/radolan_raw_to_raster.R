# radolan_raw_to_raster --------------------------------------------------------
#' Radolan raw to raster
#'
#' @param rbi rbi
#' @param version version (default: 3)
#'
#' @return ???
#' @export
#' @importFrom raster flip raster
#' @seealso
#'  * [get_radolan_urls],
#'  * [extract_radolan_zip_files],
#'  * [download_radolan],
#'  * [list_extracted_radolan_files].
radolan_raw_to_raster <- function(rbi, version = 3)
{
  stopifnot(version %in% 1:3)

  if (version == 1) {

    rb <- raster::raster(t(matrix(rbi, ncol = 900, nrow = 900)))

    return(raster::flip(rb, "y"))
  }

  if (version == 2) {

    m <- matrix(rbi, ncol = 900, nrow = 900, byrow = TRUE)

    return(raster::raster(m[rev(seq_len(ncol(m))), ]))
  }

  if (version == 3) {

    m <- matrix(rbi, ncol = 900, nrow = 900, byrow = TRUE)

    return(raster::raster(m[seq.int(ncol(m), 1, -1), ]))
  }
}
