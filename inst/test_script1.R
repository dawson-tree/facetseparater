library(terra)
library(tidyverse)
library(sf)
library(lwgeom)
library(rgl)
library(htmlwidgets)
library(rasterpolygonizer)
# devtools::load_all(".")
# library(facetseparater)

# corrected_raster15_old <- rast("D:/Jashon/working/corrected_raster15p.tif")
# Does not work with 1 meter resolution!
corrected_raster15 <- rast("D:/Dawson/SnowData/corrected_raster_lowsnow_halfmeter.tif")
smoothed_buildings <- st_read("D:/Jashon/working/Interior_Built_Polys_FBanks.shp")

corrected_raster15 <- rast("C:/Users/dtree/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_lowsnow_halfmeter.tif")
smoothed_buildings <- st_read("C:/Users/dtree/Box/Snow Load Research Stuff/DataFromJashon/working/Interior_Built_Polys_FBanks.shp")

smoothed_buildings$building_id <- smoothed_buildings$bldng_d
smoothed_buildings$object_id <- smoothed_buildings$OBJECTI
smoothed_buildings <- st_transform(smoothed_buildings, crs(corrected_raster)) |>
  select(-c(bldng_d, OBJECTI))


# slope_raster <- terrain(corrected_raster15, v = "slope", unit = "degrees")





merged_raster_snow <- readRDS("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/merged_raster_snow_halfm.rds")
merged_raster_low_snow <- readRDS("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/merged_raster_low_snow_halfm.rds")

snow_depth <-  merged_raster_snow - merged_raster_low_snow
snow_clip <- clamp(snow_depth, lower = 0, upper = 1,
                   values = FALSE)

corrected_raster <- na_imputation(merged_raster_low_snow)
saveRDS(corrected_raster, "C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.rds")


par(mfrow = c(1,1))
# par(mar = c(5.1, 4.1, 4.1, 2.1))

adjust_var <- 1
kde_n_var <- 2048

fs_results <- facet_separation(id = 165,
                               buildings = smoothed_buildings,
                               raster = corrected_raster15,
                               xmin = 464170, xmax = 464215,
                               ymin = 7193020, ymax = 7193080,
                               seed = 1234,
                               adjust = adjust_var,
                               kde_n = kde_n_var,
                               n = 100L,
                               min_inliers = 10L,
                               threshold = 0.1,
                               quiet = FALSE,
                               quiet3d = TRUE)

#######################

corrected_raster <- readRDS("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.rds")
smoothed_buildings <- st_read("D:/Jashon/working/Interior_Built_Polys_FBanks.shp")

corrected_raster <- readRDS("C:/Users/dtree/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.rds")
smoothed_buildings <- st_read("C:/Users/dtree/Box/Snow Load Research Stuff/DataFromJashon/working/Interior_Built_Polys_FBanks.shp")

corrected_raster <- rast("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.tif")
smoothed_buildings <- st_read("D:/Jashon/working/Interior_Built_Polys_FBanks.shp")

smoothed_buildings$building_id <- smoothed_buildings$bldng_d
smoothed_buildings$object_id <- smoothed_buildings$OBJECTI
smoothed_buildings <- st_transform(smoothed_buildings, crs(corrected_raster)) |>
  select(-c(bldng_d, OBJECTI))


par(mfrow = c(3,3))

fs_results <- facet_separation(id = 165,
                               buildings = smoothed_buildings,
                               raster = corrected_raster,
                               seed = 1234,
                               adjust = 1,
                               kde_n = 2048,
                               ransac_n = 500L,
                               min_inliers = 10L,
                               threshold = 0.1,
                               quiet = FALSE,
                               plot = TRUE,
                               plot3d = FALSE)



fs_results <- facet_separation(id = 711,
                               buildings = smoothed_buildings,
                               raster = corrected_raster,
                               plot3d = FALSE)
fs_results <- facet_separation(167,
                               smoothed_buildings,
                               corrected_raster,
                               plot3d = FALSE)



fs_results <- facet_separation(169,
                               smoothed_buildings,
                               corrected_raster,
                               plot3d = FALSE)
fs_results <- facet_separation(959,
                               smoothed_buildings,
                               corrected_raster,
                               plot3d = FALSE)
fs_results <- facet_separation(305,
                               smoothed_buildings,
                               corrected_raster,
                               plot3d = FALSE)



fs_results <- facet_separation(208,
                               smoothed_buildings,
                               corrected_raster,
                               plot3d = FALSE)
fs_results <- facet_separation(378,
                               smoothed_buildings,
                               corrected_raster,
                               plot3d = FALSE)
fs_results <- facet_separation(217,
                               smoothed_buildings,
                               corrected_raster,
                               plot3d = FALSE)


