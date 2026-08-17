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


#' Load a raster from the 'facetseparater' package
#'
#' @importFrom terra rast
#'
#' @export
load_raster <- function(raster_name) {
  file_name <- paste0(raster_name, ".tif")
  file_path <- system.file("extdata", file_name,
                             package = "facetseparater")

  if (file_path == "") {
    stop(paste0("Raster file '", file_name, "' does not exist."))
  }

  terra::rast(file_path)
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
