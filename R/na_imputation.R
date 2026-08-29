#' Perform NA imputation on a raster
#'
#' Perform NA imputation on a raster by fitting a quadratic surface with the 3x3
#'     area surrounding a missing value. This NA imputation strategy is ideal
#'     for accurately replacing missing rooftop pixel values.
#'
#' @param raster A 'terra::SpatRaster' object.
#'
#' @return A 'terra::SpatRaster' object corrected with this missing value
#'     imputation strategy.
na_imputation <- function(raster) {
  corrected_raster <- focal(
    raster,
    w = 3,
    fun = function(x) {
      # x[5] is the center focal cell
      # If the focal cell is already valid, keep its original value
      if (!is.na(x[5])) {
        return(x[5])
      }

      mean(
        c(
          x[2] + mean(
            c(
              x[4] - x[1],
              x[6] - x[3]
            ),
            na.rm = TRUE
          ),
          x[4] + mean(
            c(
              x[2] - x[1],
              x[8] - x[7]
            ),
            na.rm = TRUE
          ),
          x[6] + mean(
            c(
              x[2] - x[3],
              x[8] - x[9]
            ),
            na.rm = TRUE
          ),
          x[8] + mean(
            c(
              x[4] - x[7],
              x[6] - x[9]
            ),
            na.rm = TRUE
          )
        ),
        na.rm = TRUE
      )
    }
  )

  corrected_raster
}
