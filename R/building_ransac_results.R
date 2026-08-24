#' Get RANSAC algorithm results for multiple roof facets
#'
#' Perform the RANSAC algorithm to estimate the slope and aspect of multiple
#'     roof facets from one building.
#'
#' @param facets A 'terra::SpatRaster' object that represents the various facets
#'     on a building rooftop. This type of object is the output of the
#'     'kde_filter' function.
#' @param raster An elevation raster of the class 'terra::SpatRaster'.
#' @param building_id The 'building_id' for the specific building rooftop to be
#'     analyzed.
#' @param n Number of RANSAC iterations for each facet
#' @param min_inliers Minimum number of inliers required to accept a plane.
#' @param quiet Logical argument indicating whether progress messages should be
#'     printed.
#'
#' @return A list containing the following:
#' \describe{
#'   \item{results_list}{A list of lists with detailed results for each roof
#'       facet.}
#'   \item{summary_table}{Data frame summarizing slope, aspect, snow depth,
#"       inliers, and stability metrics for each roof facet.}
#'   \item{facet_polys}{A terra::SpatVector of polygons representing each of the
#"       final roof facets}
#' }
building_ransac_results <- function(facets,
                                    raster,
                                    building_id,
                                    n = 500L,
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

  total_building_results <- roof_facet_slope_ransac(
                                       raster      = raster,
                                       facets      = facets,
                                       building_id = building_id,
                                       n_iter      = n,
                                       thresh      = 0.1,
                                       min_inliers = min_inliers,
                                       quiet = quiet
                                     )

  building_results <- total_building_results[['patches']]

  names(building_results[['results_list']]) <-
    seq_along(building_results[['results_list']])

  for (i in seq_along(building_results[['results_list']])) {
    building_results[['results_list']][[i]][['facet_no']] <- i
  }

  building_results
}
