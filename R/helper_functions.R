#' Plot raster and buildings
#'
#' Plot a raster and building polygons within a specified extent.
#'
#' @param raster A raster to plot
#'
#'
#' @importFrom terra crop
#'
#' @export
plot_raster <- function(raster,
                        buildings,
                        xmin,
                        xmax,
                        ymin,
                        ymax,
                        title = "") {
  cropped_raster <- terra::crop(raster, ext(xmin, xmax, ymin, ymax))

  plot(cropped_raster,
       xlim = c(xmin, xmax),
       ylim = c(ymin, ymax),
       main = title)
  lines(buildings, col = "red", lwd = 2)
}

fun_w_cutoff <- function(x, fun, cutoff) {
  if (mean(!is.na(x)) < cutoff) {
    return(NA)
  }
  else {
    return(fun(x, na.rm = TRUE))
  }
}
