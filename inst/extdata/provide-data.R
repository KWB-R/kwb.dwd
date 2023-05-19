#
# Provide shapes of Germany as datasets
#

# Download this file:
# https://geodata.ucdavis.edu/gadm/gadm4.0/shp/gadm40_DEU_shp.zip
zip_file <- kwb.dwd:::download_dir("gadm40_DEU_shp.zip")

# TODO: Find alternatives in case that the above URL cannot be accessed
# url <- "https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2/gis-files/germany-shapefile/at_download/file/Germany_shapefile.zip"
# url <- "https://www.arcgis.com/sharing/rest/content/items/ae25571c60d94ce5b7fcbf74e27c00e0/data/vg2500_geo84.zip"

stopifnot(file.exists(zip_file))

target_dir <- kwb.dwd:::temp_dir("shapes_germany")

files <- kwb.dwd:::unzip_zip_file(zip_file, target_dir)

shp_files <- grep("\\.shp$", files, value = TRUE)

# Read shapes at different levels of detail
shapes_germany <- lapply(
  X = stats::setNames(
    shp_files,
    kwb.utils::removeExtension(basename(shp_files))
  ),
  FUN = kwb.dwd:::read_shape_file,
  use_sf = TRUE
)

(shapes_germany_0 <- shapes_germany$gadm40_DEU_0)
(shapes_germany_1 <- shapes_germany$gadm40_DEU_1)
(shapes_germany_2 <- shapes_germany$gadm40_DEU_2)

usethis::use_data(shapes_germany_0, overwrite = TRUE)
usethis::use_data(shapes_germany_1, overwrite = TRUE)
usethis::use_data(shapes_germany_2, overwrite = TRUE)
