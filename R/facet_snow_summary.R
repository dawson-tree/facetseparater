#' Summarize the characteristics of a roof facet, including snow depth
#'
#' Calculate mean snow depth for a roof facet and return the results, as well as
#'     other important facet characteristics, such as slope and aspect.
#'
#' @param fs_results A list that is the output from the 'facet_separation'
#'     function and contains facet characteristic information in the elements
#'     'results_list', 'summary_table', and 'facet_polys'.
#' @param snow_depth A 'terra::SpatRaster' object with pixel values representing
#'     snow depth.
#' @param cutoff The minimum proportion of non-missing pixel values required for
#'     a valid snow depth estimate for a roof facet.
#'
#' @examples
#' fs_results <- facet_separation(165,
#'                                smoothed_buildings,
#'                                corrected_raster)
#' snow_depth <- raster_snow - raster_no_snow
#' fs_summary <- facet_snow_summary(fs_results = fs_results,
#'                                  snow_depth = snow_depth,
#'                                  cutoff = 0.5)
#'
#' fs_summary <- facet_snow_summary(fs_results,
#'                                  snow_depth,
#'                                  0.8)
#'
#' @importFrom terra extract
#' @importFrom dplyr rename select
#'
#' @export
facet_snow_summary <- function(fs_results,
                               snow_depth,
                               cutoff = 0.8) {

  facet_snow_sum <- cbind(fs_results$summary_table,
                          terra::extract(snow_depth,
                                         fs_results[['facet_polys']],
                                         fun = function(x) {
                                           fun_w_cutoff(x, mean,
                                                        cutoff = cutoff)
                                           })) |>
    dplyr::rename(snow_depth = lyr.1) |>
    dplyr::select(-ID)

  facet_snow_sum
}
