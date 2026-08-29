#' Separate a building roof into separate facets
#'
#' Extract the slope for each pixel in a raster and use kernel density
#'     estimation to filter out the pixels that are most likely to contain facet
#'     edges, then create roof facets out of the remaining pixels and perform
#'     the RANSAC algorithm on each facet to determine facet slope and aspect,
#'     with options to plot raster information and resulting facets.
#'
#' @param id The 'building_id' for the specific building rooftop to be
#'     split into facets.
#' @param buildings An object of the classes 'sf::sf' and 'data.frame' that was
#'     created using the workflow from the package 'rasterpolygonizer' that
#'     represents the interior polygon of a set of buildings.
#' @param raster An elevation raster of the class 'terra::SpatRaster'.
#' @param seed Seed for reproducibility.
#' @param adjust The 'adjust' argument given to the 'density' function used for
#'     kernel density estimation. More information can be found in the 'density'
#'     function documentation.
#' @param kde_n The 'n' argument given to the 'density' function used for kernel
#'     density estimation. More information can be found in the 'density'
#'     function documentation.
#' @param ransac_n Number of RANSAC iterations for each facet.
#' @param threshold Residual threshold for RANSAC inlier classification in
#'     meters.
#' @param min_inliers Minimum number of inliers required to accept a plane
#'     generated using RANSAC.
#' @param quiet Logical argument indicating whether summary messages from
#'     'kde_filter' and progress messages from 'building_ransac_results' should
#'     be printed.
#' @param plot Logical argument indicating whether graphics showing building
#'     elevation, building slope, and the final separated facets should be
#'     plotted.
#' @param plot3d Logical argument indicating whether the building's roof and its
#'     facets should be plotted in three dimensions.
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
#'
#' @examples
#' fs_results <- facet_separation(id = 4642,
#'                                buildings = sample_buildings,
#'                                raster = sample_raster,
#'                                seed = 1234,
#'                                adjust = 1,
#'                                kde_n = 2048,
#'                                ransac_n = 500L,
#'                                threshold = 0.1,
#'                                min_inliers = 10L,
#'                                quiet = FALSE,
#'                                plot = TRUE,
#'                                plot3d = TRUE)
#'
#' fs_results <- facet_separation(id = 4641,
#'                                buildings = sample_buildings,
#'                                raster = sample_raster,
#'                                plot3d = FALSE)
#'
#' fs_results <- facet_separation(3696,
#'                                sample_buildings,
#'                                sample_raster)
#'
#' @importFrom dplyr filter
#' @importFrom terra terrain crop res
#' @importFrom sf st_buffer
#'
#' @export
facet_separation <- function(id,
                             buildings,
                             raster,
                             seed = 1234,
                             adjust = 1,
                             kde_n = 2048,
                             ransac_n = 500L,
                             threshold = 0.1,
                             min_inliers = 10L,
                             quiet = FALSE,
                             plot = TRUE,
                             plot3d = TRUE){

  set.seed(seed)

  building <- dplyr::filter(buildings, building_id == id)
  raster_res <- max(terra::res(raster))
  building_buffer <- sf::st_buffer(building, dist = 2*raster_res)
  building_raster <- terra::crop(raster, building_buffer, mask = TRUE)

  slope_raster <- terra::terrain(building_raster,
                                 v = "slope",
                                 unit = "degrees") |>
    crop(building, mask = TRUE)


  facet_polys <- kde_filter(building,
                            slope_raster,
                            n = kde_n,
                            adjust = adjust,
                            quiet = quiet)

  building_results <- building_ransac_results(facet_polys,
                                              raster,
                                              building_id = id,
                                              n_iter = ransac_n,
                                              thresh = threshold,
                                              min_inliers = min_inliers,
                                              quiet = quiet)

  if (plot) {
    raster_title = paste0("Elevation Raster - Building ",
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

    title = paste0("Final Facets - Building ",
                   building$building_id)
    plot_raster(building_results[['facet_polys']],
                building,
                title = title)
    text(building_results[['facet_polys']],
         labels = names(building_results[['results_list']]),
         cex = 1.5,
         font = 2)
  }

  if (plot3d) {
    plot_facets_3d(results_obj = building_results,
                   raster = raster,
                   building = building,
                   thresh = 0.1, grid_res = 30, snapshot = FALSE)
  }

  building_results
}
