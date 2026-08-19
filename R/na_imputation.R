



na_imputation <- function(raster,
                          w = 3) {
  corrected_raster <- focal(
    raster,
    w = w,
    fun = function(x) {
      # x[5] is the center focal cell
      # If the focal cell is already valid, keep its original value
      if (!is.na(x[5])) {
        return(x[5])
      }

      mean(
        c(x[2] + mean(
          c(x[4] - x[1],
            x[6] - x[3]),
          na.rm = TRUE),
          x[4] + mean(
            c(x[2] - x[1],
              x[8] - x[7]),
            na.rm = TRUE),
          x[6] + mean(
            c(x[2] - x[3],
              x[8] - x[9]),
            na.rm = TRUE),
          x[8] + mean(
            c(x[4] - x[7],
              x[6] - x[9]),
            na.rm = TRUE)),
        na.rm = TRUE
      )
    }
  )

  corrected_raster
}





# Diff between 4 and 5 is the mean of diff 1-2 and diff 7-8
# Don't use this on rasters that have a ton of NAs


# na_imputation <- function(raster,
#                           w = 3) {
#   corrected_raster <- focal(
#     raster,
#     w = w,
#     fun = function(x) {
#       # x[5] is the center focal cell
#       focal_val <- x[5]
#
#       # If the focal cell is already valid, keep its original value
#       if (!is.na(focal_val)) {
#         return(focal_val)
#       }
#
#
#       diff4to5 <- mean(
#         c(x[2] - x[1],
#           x[8] - x[7]),
#         na.rm = TRUE)
#       diff6to5 <- mean(
#         c(x[2] - x[3],
#           x[8] - x[9]),
#         na.rm = TRUE)
#       diff2to5 <- mean(
#         c(x[4] - x[1],
#           x[6] - x[3]),
#         na.rm = TRUE)
#       diff8to5 <- mean(
#         c(x[4] - x[7],
#           x[6] - x[9]),
#         na.rm = TRUE)
#
#       new_focal_val <- mean(
#         c(x[2] + diff2to5,
#           x[4] + diff4to5,
#           x[6] + diff6to5,
#           x[8] + diff8to5),
#         na.rm = TRUE
#       )
#
#       new_focal_val
#     }
#   )
#
#   corrected_raster
# }






















# na_imputation <- function(raster,
#                           w = 3) {
#   corrected_raster <- focal(
#     raster,
#     w = w,
#     fun = function(x) {
#       # x[5] is the center focal cell
#       # x[-5] extracts the 8 surrounding neighbors
#       focal_val <- x[5]
#
#       # If the focal cell is already valid, keep its original value
#       if (!is.na(focal_val)) {
#         return(focal_val)
#       }
#
#       # Extract neighbors and count non-NA values
#       neighbors <- x[c(2,4,6,8)]
#       valid_count <- sum(!is.na(neighbors))
#
#       # Only fill if 6 or more neighbors are valid
#       if (valid_count >= 3) {
#         return(mean(neighbors, na.rm = TRUE))
#       } else {
#         return(NA)
#       }
#     }
#   )
#
#   corrected_raster
# }



# na_imputation <- function(raster,
#                           w = 3) {
#   corrected_raster <- focal(
#     raster,
#     w = w,
#     fun = function(x) {
#       # x[5] is the center focal cell
#       # x[-5] extracts the 8 surrounding neighbors
#       focal_val <- x[5]
#
#       # If the focal cell is already valid, keep its original value
#       if (!is.na(focal_val)) {
#         return(focal_val)
#       }
#
#       # Extract neighbors and count non-NA values
#       neighbors <- x[-5]
#       valid_count <- sum(!is.na(neighbors))
#
#       # Only fill if 6 or more neighbors are valid
#       if (valid_count >= 6) {
#         return(mean(neighbors, na.rm = TRUE))
#       } else {
#         return(NA)
#       }
#     }
#   )
#
#   corrected_raster
# }
