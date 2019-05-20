# radolan_raw_to_raster --------------------------------------------------------
radolan_raw_to_raster <- function(rbi, version = 1)
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
