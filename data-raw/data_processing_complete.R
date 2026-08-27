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

corrected_raster <- rast("C:/Users/dtree/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.tif")
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





# 1. Get the extent of the raster as an sf polygon
raster_extent_sf <- st_as_sfc(st_bbox(corrected_raster_old))

# 2. Trim/Crop the sf object
trimmed_sf <- st_crop(osm, raster_extent_sf)




osm <- read_sf("D:/Jashon/fairbanks_osm/fairbanks_arcgis-selected/fairbanks_all.shp")
osm <- sf::st_transform(osm ,
                         terra::crs(corrected_raster))

osm

fnsb <- read_sf("D:/Jashon/working/Fairbanks_fnsbref_polys.gpkg")
fnsb <- sf::st_transform(osm ,
                         terra::crs(corrected_raster))

corrected_raster_old <- rast("D:/Jashon/working/corrected_raster15p.tif")
corrected_raster_old <- project(corrected_raster_old, corrected_raster)



############# Exploration Stuff 8/24

corrected_raster <- rast("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.tif")
valid_polys <- readRDS("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/valid_polys_defaults.rds")

subset <- terra::crop(corrected_raster,
                      ext(464100, 464300,
                          7192950, 7193150))





raw_buildings <- read_sf("D:/Jashon/fairbanks_osm/fairbanks_arcgis-selected/fairbanks_all.shp")

raw_buildings <- sf::st_transform(raw_buildings,
                                  terra::crs(corrected_raster))

edges <- extract_building_edges_to_polygons(subset,
                                            thr_prob = 0.8) # Does 0.8 need to be higher?
                                                            # A tiny bit lower

plot(edges$closed_edges)

buildings_sf <- clean_building_polygons(
  closed_edges = edges$closed_edges,
  shrink_dist  = -0.5,
  simplify_tol = 0.5,
  min_area     = 19.99,
  max_area     = 3000
)                         # Bigger simplify should help


buildings_sf <- sf::st_transform(buildings_sf,
                                 terra::crs(corrected_raster))

plot(subset)
lines(buildings_sf, col = "red")


filtered <- filter_by_ground_truth(
  buildings_sf,
  raw_buildings,
  threshold = 0.75
)

plot(subset)
lines(filtered, col = "red")

valid_polys <- remove_invalid_polys(filtered, subset, ground_tol = 1)

plot(subset)
lines(valid_polys, col = "red")











###### Code for the final operation

corrected_raster <- rast("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.tif")

raw_buildings <- read_sf("D:/Jashon/fairbanks_osm/fairbanks_arcgis-selected/fairbanks_all.shp")

raw_buildings <- sf::st_transform(raw_buildings,
                                  terra::crs(corrected_raster))

# # 1. Get the extent of the raster as an sf polygon
# raster_extent_sf <- st_as_sfc(st_bbox(merged_raster_snow))
#
# # 2. Trim/Crop the sf object
# trimmed_sf <- st_intersection(raw_buildings, raster_extent_sf)


edges <- extract_building_edges_to_polygons(corrected_raster,
                                            thr_prob = 0.75)

# Zoom in and look at part of edges$closed_edges
nhood <- terra::crop(edges$closed_edges,
                     ext(464100, 464300,
                         7192950, 7193150))

plot(nhood)

nhood2 <- terra::crop(edges$closed_edges,
                      ext(468250, 468450,
                          7193550, 7193750))

plot(nhood2)

bigbldngs <- terra::crop(edges$closed_edges,
                        ext(465850, 466150,
                            7193100, 7193300))

plot(bigbldngs)


# Try with 0.7

edges0.7 <- extract_building_edges_to_polygons(corrected_raster,
                                            thr_prob = 0.7)

# Zoom in and look at part of edges$closed_edges
nhood0.7 <- terra::crop(edges0.7$closed_edges,
                     ext(464100, 464300,
                         7192950, 7193150))

plot(nhood0.7)

nhood2.0.7 <- terra::crop(edges0.7$closed_edges,
                      ext(468250, 468450,
                          7193550, 7193750))

plot(nhood2.0.7)

bigbldngs0.7 <- terra::crop(edges0.7$closed_edges,
                         ext(465850, 466150,
                             7193100, 7193300))

plot(bigbldngs0.7)

################################

buildings_sf <- clean_building_polygons(
  closed_edges = bigbldngs,
  shrink_dist  = -0.5,
  simplify_tol = 0.5,
  min_area     = 19.99,
  max_area     = 3000
)

plot(st_geometry(buildings_sf))

buildings_sf0.7 <- clean_building_polygons(
  closed_edges = bigbldngs0.7,
  shrink_dist  = -0.5,
  simplify_tol = 0.5,
  min_area     = 19.99,
  max_area     = 3000
)

plot(st_geometry(buildings_sf0.7))

#################################### Actual final results (0.75)
library(sf)
library(terra)
library(rasterpolygonizer)
devtools::load_all(".")

corrected_raster_test <- rast("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.tif")

raw_buildings <- read_sf("D:/Jashon/fairbanks_osm/fairbanks_arcgis-selected/fairbanks_all.shp")

raw_buildings <- sf::st_transform(raw_buildings,
                                  terra::crs(corrected_raster))


edges <- extract_building_edges_to_polygons(corrected_raster,
                                            thr_prob = 0.75)

saveRDS(edges, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/edges_75.rds")

buildings_sf <- clean_building_polygons(
  closed_edges = edges$closed_edges,
  shrink_dist  = -0.5,
  simplify_tol = 0.5,
  min_area     = 19.99,
  max_area     = 3000
)

buildings_sf <- sf::st_transform(buildings_sf,
                                 terra::crs(corrected_raster))

saveRDS(buildings_sf, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/buildings_sf_75.rds")


filtered <- filter_by_ground_truth(
  buildings_sf,
  raw_buildings,
  threshold = 0.75
)

saveRDS(filtered, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/filtered_75.rds")

valid_polys <- remove_invalid_polys(filtered, corrected_raster, ground_tol = 1)

saveRDS(valid_polys, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/valid_polys_75.rds")






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

################################################################################


nhood0.7 <- terra::crop(corrected_raster,
                        ext(464100, 464300,
                            7192950, 7193150))

plot(nhood0.7)
lines(valid_polys, col="red")

nhood2.0.7 <- terra::crop(corrected_raster,
                          ext(468250, 468450,
                              7193550, 7193750))

plot(nhood2.0.7)
lines(valid_polys, col="red")

bigbldngs0.7 <- terra::crop(corrected_raster,
                            ext(465850, 466150,
                                7193100, 7193300))

plot(bigbldngs0.7)
lines(valid_polys, col="red")


plot(corrected_raster)
lines(valid_polys, col="red")

nhood3.0.7 <- terra::crop(corrected_raster,
                          ext(468800, 469100,
                              7194800, 7195100))

plot(nhood3.0.7)
lines(valid_polys, col="red")


cr_167 <- terra::crop(corrected_raster,
                      ext(464165, 464180,
                          7193050, 7193070))

plot(cr_167)
lines(valid_polys, col="red")



plot_raster_area <- function(raster,
                             buildings,
                             xmin, xmax,
                             ymin, ymax,
                             title = "") {

  cropped_raster <- terra::crop(raster, ext(xmin, xmax, ymin, ymax))

  plot(cropped_raster,
       xlim = c(xmin, xmax),
       ylim = c(ymin, ymax),
       main = title)
  lines(buildings, col = "red", lwd = 2)
  text(buildings,
       labels = buildings$building_id,
       cex = 1.5,
       font = 2)
}
par(mfrow = c(3,5))

# plot_raster_area(corrected_raster,
#                  valid_polys,
#                  464175, 464205,
#                  7193030, 7193070)
facet_separation(5895,
                 valid_polys,
                 corrected_raster)

# plot_raster_area(corrected_raster,
#                  valid_polys,
#                  466000, 466035,
#                  7193040, 7193140)
facet_separation(25191,
                 valid_polys,
                 corrected_raster)

# erode messes this one up
# plot_raster_area(corrected_raster,
#                  valid_polys,
#                  464165, 464180,
#                  7193050, 7193070)
facet_separation(4629,
                 valid_polys,
                 corrected_raster)

# plot_raster_area(corrected_raster,
#                  valid_polys,
#                  464150, 464170,
#                  7193050, 7193080)
facet_separation(4630,
                 valid_polys,
                 corrected_raster)

# plot_raster_area(corrected_raster,
#                  valid_polys,
#                  468445, 468470,
#                  7193450, 7193470)
facet_separation(116950,
                 valid_polys,
                 corrected_raster)

# plot_raster_area(corrected_raster,
#                  valid_polys,
#                  464310, 464325,
#                  7192930, 7192955)
facet_separation(7721,
                 valid_polys,
                 corrected_raster)

# erode is not working
# plot_raster_area(corrected_raster,
#                  valid_polys,
#                  464015, 464040,
#                  7193205, 7193230)
facet_separation(1853,
                 valid_polys,
                 corrected_raster)

# plot_raster_area(corrected_raster,
#                  valid_polys,
#                  464935, 464960,
#                  7192870, 7192895)
facet_separation(15197,
                 valid_polys,
                 corrected_raster)

# plot_raster_area(corrected_raster,
#                  valid_polys,
#                  464165, 464180,
#                  7193230, 7193255)
facet_separation(4648,
                 valid_polys,
                 corrected_raster)




################# Facet Snow Summary ###########################################
library(terra)
library(dplyr)
devtools::load_all(".")

corrected_raster <- rast("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.tif")
valid_polys <- readRDS("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/valid_polys_70.rds")
snow_clip <- rast("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/snow_clip.tif")


all_facet_results <- lapply(valid_polys$building_id,
                            function(x) {
                              fs_results <- facet_separation(x,
                                                             valid_polys,
                                                             corrected_raster,
                                                             quiet = TRUE,
                                                             plot = FALSE,
                                                             plot3d = FALSE)
                              fs_results
                            })

saveRDS(all_facet_results, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/all_facet_results_halfm.rds")


all_summary_results_0.8 <- lapply(all_facet_results,
                                  function(fs_results) {
                                    tryCatch({
                                      snow_depths <- facet_snow_summary(fs_results,
                                                                        snow_clip,
                                                                        cutoff = 0.8)
                                      return(snow_depths)
                                    }, error = function(e) {
                                      # message("Error processing a facet: ", e$message)
                                      return(NULL)
                                    })
                                  }) |>
  bind_rows()


(length(all_summary_results_0.8$snow_depth) -
    sum(is.na(all_summary_results_0.8$snow_depth))) / length(all_summary_results_0.8$snow_depth)

saveRDS(all_summary_results_0.8, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/all_summary_results_halfm_80.rds")


############

all_summary_results_0.7 <- lapply(all_facet_results,
                                  function(fs_results) {
                                    tryCatch({
                                      snow_depths <- facet_snow_summary(fs_results,
                                                                        snow_clip,
                                                                        cutoff = 0.7)
                                      return(snow_depths)
                                    }, error = function(e) {
                                      return(NULL)
                                    })
                                  }) |>
  bind_rows()


(length(all_summary_results_0.7$snow_depth) -
    sum(is.na(all_summary_results_0.7$snow_depth))) / length(all_summary_results_0.7$snow_depth)

saveRDS(all_summary_results_0.7, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/all_summary_results_halfm_70.rds")


############

all_summary_results_0.5 <- lapply(all_facet_results,
                                  function(fs_results) {
                                    tryCatch({
                                      snow_depths <- facet_snow_summary(fs_results,
                                                                        snow_clip,
                                                                        cutoff = 0.5)
                                      return(snow_depths)
                                    }, error = function(e) {
                                      return(NULL)
                                    })
                                  }) |>
  bind_rows()


(length(all_summary_results_0.5$snow_depth) -
    sum(is.na(all_summary_results_0.5$snow_depth))) / length(all_summary_results_0.5$snow_depth)

saveRDS(all_summary_results_0.5, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/all_summary_results_halfm_50.rds")
