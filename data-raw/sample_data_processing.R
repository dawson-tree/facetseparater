library(terra)
library(sf)
library(dplyr)

sample_raster <- crop(
  corrected_raster,
  ext(
    464115, 464188,
    7193148, 7193192
  )
)
terra::writeRaster(sample_raster, "inst/extdata/sample_raster.tif")

sample_buildings <- sf::st_crop(valid_polys, st_bbox(sample_raster)) |>
  select(-perim, -perim_area_ratio)
usethis::use_data(sample_buildings, overwrite = TRUE)

sample_snow_depth <- crop(
  snow_clip,
  ext(sample_raster)
)
terra::writeRaster(sample_snow_depth, "inst/extdata/sample_snow_depth.tif")
