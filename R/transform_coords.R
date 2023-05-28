# transform_coords -------------------------------------------------------------
transform_coords <- function(x, target_crs = NULL, use_sf = inherits(x, "sf"))
{
  if (is.null(target_crs)) {
    return(x)
  }

  if (use_sf) {

    sf::st_transform(x, target_crs)

  } else {

    sp::spTransform(x, target_crs)
  }
}
