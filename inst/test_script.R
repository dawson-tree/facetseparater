library(terra)
devtools::load_all(".")

corrected_raster <- rast("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/corrected_raster_na_imputation.tif")
valid_polys <- readRDS("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/valid_polys_70.rds")

par(mfrow = c(3,3))

fs_results <- facet_separation(id = 5895,
                               buildings = valid_polys,
                               raster = corrected_raster,
                               seed = 1234,
                               adjust = 1,
                               kde_n = 2048,
                               ransac_n = 500L,
                               threshold = 0.1,
                               min_inliers = 10L,
                               quiet = FALSE,
                               plot = TRUE,
                               plot3d = FALSE)
fs_results <- facet_separation(25191,
                               valid_polys,
                               corrected_raster,
                               plot3d = FALSE)
fs_results <- facet_separation(4629,
                               valid_polys,
                               corrected_raster,
                               plot3d = FALSE)

fs_results <- facet_separation(4630,
                               valid_polys,
                               corrected_raster,
                               plot3d = FALSE)
fs_results <- facet_separation(116950,
                               valid_polys,
                               corrected_raster,
                               plot3d = FALSE)
fs_results <- facet_separation(7721,
                               valid_polys,
                               corrected_raster,
                               plot3d = FALSE)

fs_results <- facet_separation(1853,
                               valid_polys,
                               corrected_raster,
                               plot3d = FALSE)
fs_results <- facet_separation(15197,
                               valid_polys,
                               corrected_raster,
                               plot3d = FALSE)
fs_results <- facet_separation(4648,
                               valid_polys,
                               corrected_raster,
                               plot3d = FALSE)


fs_results <- facet_separation(id = 5895,
                               buildings = valid_polys,
                               raster = corrected_raster,
                               seed = 1234,
                               adjust = 1,
                               kde_n = 2048,
                               ransac_n = 500L,
                               threshold = 0.1,
                               min_inliers = 10L,
                               quiet = FALSE,
                               plot = TRUE,
                               plot3d = FALSE)

snow_clip <- rast("C:/Users/A02324772/Box/Snow Load Research Stuff/Data/SnowData/snow_clip.tif")

fs_snow_results <- facet_snow_summary(fs_results,
                                      snow_clip)
