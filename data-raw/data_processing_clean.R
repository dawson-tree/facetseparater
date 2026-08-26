library(sf)
library(terra)
library(rasterpolygonizer)
devtools::load_all(".")

# 3/11/2023
tif_dir_snow <- "C:/Users/A02324772/Desktop/Snow Load Research Data/LIDAR/snow_rasters_0.5m"

# 10/24/2023
tif_dir_low_snow <- "C:/Users/A02324772/Desktop/Snow Load Research Data/LIDAR/lowsnow_rasters_0.5m"


tif_files_snow <- list.files(tif_dir_snow, pattern = "\\.tif$",
                             full.names = TRUE)
tif_files_low_snow <- list.files(tif_dir_low_snow, pattern = "\\.tif$",
                                 full.names = TRUE)

rasters_snow <- lapply(tif_files_snow, rast)
rasters_low_snow <- lapply(tif_files_low_snow, rast)
total_ext <- Reduce(terra::union, lapply(rasters_snow, ext))

# Force to standard 2D UTM zone 6N
target_crs <- "EPSG:32606"

rasters_snow <- lapply(rasters_snow, function(r) {
  crs(r) <- target_crs
  r
})

rasters_low_snow <- lapply(rasters_low_snow, function(r) {
  crs(r) <- target_crs
  r
})


template_snow <- rast(
  ext = total_ext,
  resolution = c(0.5, 0.5),
  crs = crs(rasters_snow[[1]])  # Use CRS from first raster
)

template_low_snow <- rast(
  ext = total_ext,
  resolution = c(0.5, 0.5),
  crs = crs(rasters_low_snow[[1]])  # Use CRS from first raster
)



rasters_resampled_snow <- lapply(rasters_snow, function(r) {
  resample(r, template_snow, method = "bilinear")
})

rasters_resampled_low_snow <- lapply(rasters_low_snow, function(r) {
  resample(r, template_low_snow, method = "bilinear")
})



stacked_snow <- rast(rasters_resampled_snow)
stacked_low_snow <- rast(rasters_resampled_low_snow)


merged_raster_snow <- app(stacked_snow, fun = function(x)
  mean(x, na.rm = TRUE))
merged_raster_low_snow <- app(stacked_low_snow, fun = function(x)
  mean(x, na.rm = TRUE))



snow_depth <-  merged_raster_snow - merged_raster_low_snow
snow_clip <- clamp(snow_depth, lower = 0, upper = 1,
                   values = FALSE)

corrected_raster <- na_imputation(merged_raster_low_snow)

terra::writeRaster(merged_raster_snow, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/merged_raster_snow_halfm.tif", overwrite = TRUE)
terra::writeRaster(merged_raster_low_snow, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/merged_raster_low_snow_halfm.tif", overwrite = TRUE)
terra::writeRaster(snow_clip, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/snow_clip.tif", overwrite = TRUE)
terra::writeRaster(corrected_raster, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.tif", overwrite = TRUE)



#################################### Actual final results (0.7)
library(sf)
library(terra)
library(rasterpolygonizer)
devtools::load_all(".")

corrected_raster <- rast("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.tif")

raw_buildings <- read_sf("D:/Jashon/fairbanks_osm/fairbanks_arcgis-selected/fairbanks_all.shp")

raw_buildings <- sf::st_transform(raw_buildings,
                                  terra::crs(corrected_raster))


edges <- extract_building_edges_to_polygons(corrected_raster,
                                            thr_prob = 0.7)

saveRDS(edges, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/edges_70.rds")

buildings_sf <- clean_building_polygons(
  closed_edges = edges$closed_edges,
  shrink_dist  = -0.5,
  simplify_tol = 0.5,
  min_area     = 19.99,
  max_area     = 3000
)

buildings_sf <- sf::st_transform(buildings_sf,
                                 terra::crs(corrected_raster))

saveRDS(buildings_sf, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/buildings_sf_70.rds")


filtered <- filter_by_ground_truth(
  buildings_sf,
  raw_buildings,
  threshold = 0.75
)

saveRDS(filtered, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/filtered_70.rds")

valid_polys <- remove_invalid_polys(filtered, corrected_raster, ground_tol = 1)

saveRDS(valid_polys, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/valid_polys_70.rds")
