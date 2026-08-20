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

# saveRDS(merged_raster_snow, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/merged_raster_snow_halfm.rds")
# saveRDS(merged_raster_low_snow, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/merged_raster_low_snow_halfm.rds")
#
#
#
# merged_raster_snow <- readRDS("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/merged_raster_snow_halfm.rds")
# merged_raster_low_snow <- readRDS("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/merged_raster_low_snow_halfm.rds")

snow_depth <-  merged_raster_snow - merged_raster_low_snow
snow_clip <- clamp(snow_depth, lower = 0, upper = 1,
                   values = FALSE)

corrected_raster <- na_imputation(merged_raster_low_snow)

terra::writeRaster(merged_raster_snow, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/merged_raster_snow_halfm.tif", overwrite = TRUE)
terra::writeRaster(merged_raster_low_snow, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/merged_raster_low_snow_halfm.tif", overwrite = TRUE)
terra::writeRaster(snow_clip, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/snow_clip.tif", overwrite = TRUE)
terra::writeRaster(corrected_raster, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.tif", overwrite = TRUE)


raw_buildings <- read_sf("D:/Jashon/fairbanks_osm/fairbanks_arcgis-selected/fairbanks_all.shp")

raw_buildings <- sf::st_transform(raw_buildings,
                                         terra::crs(corrected_raster))

# 1. Get the extent of the raster as an sf polygon
raster_extent_sf <- st_as_sfc(st_bbox(merged_raster_snow))

# 2. Trim/Crop the sf object
trimmed_sf <- st_intersection(raw_buildings, raster_extent_sf)


edges <- extract_building_edges_to_polygons(corrected_raster,
                                            thr_prob = 0.8)

buildings_sf <- clean_building_polygons(
  closed_edges = edges$closed_edges,
  shrink_dist  = -0.5,
  simplify_tol = 0.5,
  min_area     = 19.99,
  max_area     = 3000
)


buildings_sf <- sf::st_transform(buildings_sf,
                                         terra::crs(corrected_raster))


filtered <- filter_by_ground_truth(
  buildings_sf,
  raw_buildings,
  threshold = 0.75
)

valid_polys <- remove_invalid_polys(filtered, corrected_raster, ground_tol = 1)

# saveRDS(valid_polys, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/valid_polys_defaults.rds")





corrected_raster <- rast("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.tif")
merged_raster_low_snow <- rast("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/merged_raster_low_snow_halfm.tif")
merged_raster_snow <- rast("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/merged_raster_snow_halfm.tif")
corrected_raster15 <- rast("D:/Dawson/SnowData/corrected_raster_lowsnow_halfmeter.tif")
valid_polys <- readRDS("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/valid_polys_defaults.rds")



plot_raster(corrected_raster,
            filter(valid_polys,
                   building_id == 6))
plot(st_geometry(raw_buildings), add = TRUE)
# plot(corrected_raster)
# plot(st_geometry(raw_buildings), add = TRUE, col = "red")





plot_raster(corrected_raster15,
            filter(valid_polys,
                   building_id == 6))
plot(st_geometry(raw_buildings), add = TRUE)

plot_raster(merged_raster_low_snow,
            filter(valid_polys,
                   building_id == 6))
plot(st_geometry(raw_buildings), add = TRUE)



cropped_raster <- terra::crop(corrected_raster,
                              ext(464175, 464210,
                                  7193030, 7193070))

plot(cropped_raster,
     xlim = c(464175, 464210),
     ylim = c(7193030, 7193070))
lines(raw_buildings, col = "red", lwd = 2)

#

cropped_raster <- terra::crop(corrected_raster,
                              ext(464175, 464210,
                                  7193030, 7193070))

plot(cropped_raster,
     xlim = c(464175, 464210),
     ylim = c(7193030, 7193070))
lines(valid_polys, col = "red", lwd = 2)
