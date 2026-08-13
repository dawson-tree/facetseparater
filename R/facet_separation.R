#' Separate a building roof into separate facets
#'
#' Extract the slope for each pixel in a raster and use kernel density
#'     estimation to filter out the pixels that are most likely to contain facet
#'     edges, then create roof facets out of the remaining pixels and perform
#'     the RANSAC algorithm on each facet to determine facet slope and aspect,
#'     with options to plot raster information and resulting facets.
#'
#' @param id The 'building_id' for the specific building rooftop to be
#'     split into facets
#' @param buildings An object of the classes 'sf::sf' and 'data.frame' that was
#'     created using the workflow from the package 'rasterpolygonizer' that
#'     represents the interior polygon of a set of buildings
#' @param raster An elevation raster of the class 'terra::SpatRaster'
#' @param
#'
#'
#'
#'
#'
#'
#'
#'
#' @importFrom dplyr filter
#' @importFrom terra terrain
#'
#' @export
facet_separation <- function(id,
                             buildings,
                             raster,
                             seed = 1234,
                             adjust = 1,
                             kde_n = 2048,
                             n = 500L,
                             min_inliers = 10L,
                             quiet = FALSE,
                             plot = TRUE,
                             plot3d = TRUE){

  set.seed(seed)

  building <- dplyr::filter(buildings, building_id == id)

  full_slope_raster <- terra::terrain(raster,
                                 v = "slope",
                                 unit = "degrees")
  slope_raster <- crop(full_slope_raster, building, mask = TRUE)

  facets <- kde_filter(building,
                       slope_raster,
                       n = kde_n,
                       adjust = adjust,
                       quiet = quiet)

  facet_polys <- as.polygons(facets, aggregate = TRUE, na.rm = TRUE)


  kde_results <- kde_building_results(facet_polys,
                                      raster,
                                      building_id = id,
                                      n = n,
                                      min_inliers = min_inliers,
                                      quiet = quiet)

  if (plot) {
    raster_title = paste0("Corrected Raster - Building ",
                         building$building_id)
    plot_raster(raster,
                 building,
                 title = raster_title)

    slope_title = paste0("Slope Raster - Building ",
                         building$building_id)
    # slope_title = paste0("Slope Raster for Building ", building$building_id,
    #                      " (Bandwidth = ", round(dens$bw, 3), ")")
    plot_raster(slope_raster,
                 building,
                 title = slope_title)


    title = paste0("Final Facets after RANSAC Algorithm - Building ",
                   building$building_id)
    plot_raster(kde_results[['facet_polys']],
                building,
                title = title)
    text(kde_results[['facet_polys']],
         labels = names(kde_results[['results_list']]),
         cex = 1.5,
         font = 2)
  }

  if (plot3d) {
    plot_full_building_plane_kde(results_obj = kde_results,
                                 raster = raster,
                                 building = building,
                                 thresh = 0.1, grid_res = 30, snapshot = FALSE)
  }

  kde_results
}
