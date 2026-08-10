#' @importFrom terra extract
#' @importFrom dplyr rename select
#'
#' @export
facet_snow_depth <- function(fs_results,
                             snow_clip,
                             cutoff) {

  facet_snow_clips <- cbind(fs_results$summary_table,
                            terra::extract(snow_clip,
                                           fs_results[['facet_polys']],
                                           fun = function(x) {
                                             fun_w_cutoff(x, mean,
                                                          cutoff = cutoff)
                                           })) |>
    dplyr::rename(snow_depth = lyr.1) |>
    dplyr::select(-ID)

  facet_snow_clips
}
