
na_imputation <- function(raster,
                          w = 3) {
  corrected_raster <- focal(
    raster,
    w = w,
    fun = function(x) {
      # x[5] is the center focal cell
      # x[-5] extracts the 8 surrounding neighbors
      focal_val <- x[5]

      # If the focal cell is already valid, keep its original value
      if (!is.na(focal_val)) {
        return(focal_val)
      }

      # Extract neighbors and count non-NA values
      neighbors <- x[-5]
      valid_count <- sum(!is.na(neighbors))

      # Only fill if 6 or more neighbors are valid
      if (valid_count >= 6) {
        return(mean(neighbors, na.rm = TRUE))
      } else {
        return(NA)
      }
    }
  )

  corrected_raster
}
