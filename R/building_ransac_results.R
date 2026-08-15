#' Get RANSAC algorithm results for multiple roof facets
#'
#' Perform the RANSAC algorithm to estimate the slope and aspect of multiple
#'     roof facets from one building.
#'
building_ransac_results <- function(facets,
                                    raster,
                                    building_id,
                                    n = 100L,
                                    min_inliers = 10L,
                                    quiet = FALSE) {

  total_building_results <- lapply(facets,
                                   function(x) {
                                     roof_facet_slope_ransac(
                                       raster      = raster,
                                       facets      = facets,
                                       building_id = building_id,
                                       n_iter      = n,
                                       thresh      = 0.1,
                                       min_inliers = min_inliers,
                                       quiet = quiet
                                     )
                                   })

  building_results <- total_building_results[['patches']]

  names(building_results[['results_list']]) <-
    seq_along(building_results[['results_list']])

  for (i in seq_along(building_results[['results_list']])) {
    building_results[['results_list']][[i]][['facet_no']] <- i
  }

  building_results
}
