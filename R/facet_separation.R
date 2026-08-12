#' Separate a building roof into separate facets
#'
#' Extract the slope for each pixel in a raster and use kernel density
#'     estimation to filter out the pixels that are most likely to contain facet
#'     edges, then create roof facets out of the remaining pixels and perform
#'     the RANSAC algorithm on each facet to determine facet slope and aspect,
#'     with options to plot the resulting facets.
#'
#'
#' @importFrom dplyr filter
#' @importFrom terra terrain
#'
#' @export
facet_separation <- function(id,
                             buildings,
                             raster,
                             xmin, xmax,
                             ymin, ymax,
                             seed = 1234,
                             adjust = 1,
                             kde_n = 2048,
                             n = 500L,
                             min_inliers = 10L,
                             quiet = FALSE,
                             quiet3d = FALSE){

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
                       xmin, xmax,
                       ymin, ymax,
                       quiet = quiet)

  facet_polys <- as.polygons(facets, aggregate = TRUE, na.rm = TRUE)


  kde_results <- kde_building_results(facet_polys,
                                      raster,
                                      building_id = id,
                                      n = n,
                                      min_inliers = min_inliers,
                                      quiet = quiet)

  if (!quiet) {
    raster_title = paste0("Corrected Raster - Building ",
                         building$building_id)
    plot_raster2(raster,
                 building,
                 # xmin, xmax,
                 # ymin, ymax,
                 title = raster_title)

    slope_title = paste0("Slope Raster - Building ",
                         building$building_id)
    # slope_title = paste0("Slope Raster for Building ", building$building_id,
    #                      " (Bandwidth = ", round(dens$bw, 3), ")")
    plot_raster2(slope_raster,
                 building,
                 # xmin, xmax,
                 # ymin, ymax,
                 title = slope_title)


    title = paste0("Final Facets after RANSAC Algorithm - Building ",
                   building$building_id)
    plot_raster2(kde_results[['facet_polys']],
                building,
                # xmin, xmax,
                # ymin, ymax,
                title = title)
    text(kde_results[['facet_polys']],
         labels = names(kde_results[['results_list']]),
         cex = 1.5,
         font = 2)
  }

  if (!quiet3d) {
    plot_full_building_plane_kde(results_obj = kde_results,
                                 raster = raster,
                                 building = building,
                                 thresh = 0.1, grid_res = 30, snapshot = FALSE)
  }

  kde_results
}
