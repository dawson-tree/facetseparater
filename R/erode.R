

erode <- function(raster) {
  corrected_raster <- focal(
    raster,
    w = 3,
    fun = function(x) {
      # x[5] is the center focal cell
      focal_val <- x[5]

      # If the focal cell is already valid, keep its original value
      if (is.na(focal_val)) {
        return(NA)
      }

      neighbors4 <- x[c(2,4,6,8)]
      neighbors8 <- x[c(1,2,3,4,6,7,8,9,1)]

      if (sum(is.na(neighbors4)) >= 2) {
        # Keep track of the times it switches from NA to valid
        switch <- 0

        for (i in (seq_len(length(neighbors8) - 1))) {
          if (is.na(x[i]) != is.na(x[i+1])) {
            switch <- switch + 1
          }
        }

        if (switch > 2) {
          return(NA)
        }

      }
      as.numeric(focal_val)
    }
  )

  corrected_raster
}
