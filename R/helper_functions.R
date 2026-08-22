#' Plot raster and buildings
#'
#' Plot a raster and building polygons within a specified extent.
#'
#' @param raster A 'terra::SpatRaster' object to be plotted
#' @param building A building in the form of an 'sf' object with one observation
#'     to be plotted. This argument also defines the area of the raster that
#'     will be plotted.
#' @param title The title of the plot.
#'
#' @importFrom terra crop
#'
#' @export
plot_raster <- function(raster,
                        building,
                        title = "") {
  bldng_ext <- ext(building) |>
    as.vector()

  xmin <- bldng_ext[1] - ((bldng_ext[2] - bldng_ext[1])/3)
  xmax <- bldng_ext[2] + ((bldng_ext[2] - bldng_ext[1])/3)
  ymin <- bldng_ext[3] - ((bldng_ext[4] - bldng_ext[3])/3)
  ymax <- bldng_ext[4] + ((bldng_ext[4] - bldng_ext[3])/3)

  cropped_raster <- terra::crop(raster, ext(xmin, xmax, ymin, ymax))

  plot(cropped_raster,
       xlim = c(xmin, xmax),
       ylim = c(ymin, ymax),
       main = title)
  lines(building, col = "red", lwd = 2)
}



fun_w_cutoff <- function(x, fun, cutoff) {
  if (mean(!is.na(x)) < cutoff) {
    return(NA)
  }
  else {
    return(fun(x, na.rm = TRUE))
  }
}





# plot_raster <- function(raster,
#                         buildings,
#                         xmin,
#                         xmax,
#                         ymin,
#                         ymax,
#                         title = "") {
#   cropped_raster <- terra::crop(raster, ext(xmin, xmax, ymin, ymax))
#
#   plot(cropped_raster,
#        xlim = c(xmin, xmax),
#        ylim = c(ymin, ymax),
#        main = title)
#   lines(buildings, col = "red", lwd = 2)
# }
