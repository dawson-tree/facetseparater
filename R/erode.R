

# It can mess up really small buildings
erode <- function(raster) {
  corrected_raster <- focal(
    raster,
    w = 3,
    fun = function(x) {
      # x[5] is the center focal cell
      # If the focal cell is already missing, return NA
      if (is.na(x[5])) {
        return(NA)
      }

      neighbors4 <- x[c(2,6,8,4)]
      neighbors8 <- x[c(1,2,3,6,9,8,7,4)]

      if (sum(is.na(neighbors4)) == 2) {

        gap_1 <- TRUE
        gap_2 <- FALSE
        gap_1_has_na <- FALSE
        gap_2_has_na <- FALSE

        for (i in (seq_len(length(neighbors8)))) {
          # If it's one of the two NAs, switch the gap
          if (i %% 2 == 0 & !is.na(neighbors8[i])) {
            gap_1 <- !gap_1
            gap_2 <- !gap_2
          } else {
            if (gap_1 & is.na(neighbors8[i])) {
              gap_1_has_na <- TRUE
            } else if (gap_2 & is.na(neighbors8[i])) {
              gap_2_has_na <- TRUE
            }
          }
        }

        if (gap_1_has_na & gap_2_has_na) {
          return(NA)
        }

      }
      as.numeric(x[5])
    }
  )

  corrected_raster
}
