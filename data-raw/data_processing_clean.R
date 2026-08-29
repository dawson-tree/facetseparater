library(sf)
library(terra)
library(rasterpolygonizer)
library(dplyr)
library(ggplot2)
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

#######################

all_results <- readRDS("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/all_summary_results_halfm_80.rds")
all_results_copy <- all_results

all_results_copy <- all_results_copy |>
  filter(slope_deg > 4.76) |>
  mutate(aspect_direction = case_when(
    aspect_deg < 45 | aspect_deg > 315 ~ "North",
    aspect_deg > 45 & aspect_deg < 135 ~ "East",
    aspect_deg > 135 & aspect_deg < 225 ~ "South",
    aspect_deg > 225 & aspect_deg < 315 ~ "West"
  ))

all_results_copy$aspect_direction <- factor(all_results_copy$aspect_direction,
                                  levels = c("North",
                                             "East",
                                             "South",
                                             "West"))



ggplot(data = all_results_copy,
       aes(x = aspect_direction, y = snow_depth, fill = aspect_direction)) +
  geom_boxplot() +
  labs(
    title = "Distributions of Snow Depth by Roof Aspect for Buildings in Fairbanks, Alaska",
    x = "Roof Aspect",
    y = "Snow Depth (m)",
    fill = "Roof Aspect"
  ) +
  scale_fill_manual(values = c("North" = "dodgerblue1",
                               "East" = "darkorange1",
                               "South" = "indianred1",
                               "West" = "palegreen3")) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 17,
      margin = margin(b = 15),
      hjust = 0.5),
    axis.title.x = element_text(
      size = 14,
      margin = margin(t = 15)),
    axis.title.y = element_text(
      size = 14,
      margin = margin(r = 15)),
    axis.text = element_text(
      size = 12,
    ),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 20)
  )

ggplot(data = all_results,
       aes(x = slope_deg, y = snow_depth)) +
  geom_point(col="dodgerblue1") +
  labs(
    title = "Snow Depth vs Roof Slope for Buildings in Fairbanks, Alaska",
    x = "Roof Slope (degrees)",
    y = "Snow Depth (m)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 17,
      margin = margin(b = 15),
      hjust = 0.5),
    axis.title.x = element_text(
      size = 14,
      margin = margin(t = 15)),
    axis.title.y = element_text(
      size = 14,
      margin = margin(r = 15)),
    axis.text = element_text(
      size = 12,
    ),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 20)
  )

cor(
  all_summary_results_0.8$snow_depth,
  all_summary_results_0.8$slope_deg,
  use = "pairwise.complete.obs")
