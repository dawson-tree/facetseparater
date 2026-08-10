
kde_building_results <- function(facets,
                                 raster,
                                 building_id,
                                 n = 100L,
                                 min_inliers = 10L,
                                 quiet = FALSE) {

  building_results <- lapply(facets,
                             function(x) {
                               kde_roof_slope_RANSAC(
                                 raster      = raster,
                                 facets      = facets,
                                 building_id = building_id,
                                 n_iter      = n,
                                 thresh      = 0.1,
                                 min_inliers = min_inliers,
                                 quiet = quiet
                               )
                             })

  building_results[['patches']]
}
