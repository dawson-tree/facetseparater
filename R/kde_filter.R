#' Filter raster pixels using kernel density estimation
#'
kde_filter <- function(building,
                       full_slope_raster,
                       n = 2048,
                       adjust = 1,
                       quiet = FALSE) {

  slope_raster <- crop(full_slope_raster, building, mask = TRUE)

  slope_vector <- values(slope_raster, mat = FALSE) |>
    na.omit()

  # 2. Compute Kernel Density Estimation
  dens <- density(slope_vector, n = n, adjust = adjust) # I'm feeling good about this

  # 3. Find the main peak (mode)
  max_peak_idx <- which.max(dens$y)
  main_peak_x  <- dens$x[max_peak_idx]

  # 4. Identify local minima (troughs) in the density curve
  # A point is a local minimum if it is smaller than both neighboring points
  is_trough <- c(FALSE,
                 diff(sign(diff(dens$y))) == 2,
                 FALSE)
  trough_indices <- which(is_trough)

  # 5. Find the immediate left and right boundaries around the main peak
  left_troughs  <- trough_indices[trough_indices < max_peak_idx]
  right_troughs <- trough_indices[trough_indices > max_peak_idx]

  # Set lower cutoff (use first trough to the left, or min value if no trough exists)
  lower_cutoff <- if (length(left_troughs) > 0) dens$x[max(left_troughs)] else min(slope_vector)

  # Set upper cutoff (use first trough to the right, or max value if no trough exists)
  upper_cutoff <- if (length(right_troughs) > 0) dens$x[min(right_troughs)] else max(slope_vector)

  # 6. Filter the original dataset to keep only the main cluster
  filtered_slope_vector <- slope_vector[slope_vector >= lower_cutoff & slope_vector <= upper_cutoff]


  clamped_slope_raster <- clamp(slope_raster,
                                lower = lower_cutoff, upper = upper_cutoff, values = FALSE)

  patch_slope_raster <- patches(clamped_slope_raster, directions = 4)

  if (!quiet) {
    # slope_title = paste0("Slope Raster - Building ",
    #                      building$building_id)
    # # slope_title = paste0("Slope Raster for Building ", building$building_id,
    # #                      " (Bandwidth = ", round(dens$bw, 3), ")")
    # plot_raster2(slope_raster,
    #             building,
    #             # xmin, xmax,
    #             # ymin, ymax,
    #             title = slope_title)

    # Print KDE summary
    cat(sprintf("Main Peak at: %.2f\n", main_peak_x))
    cat(sprintf("Boundaries: [%.2f, %.2f]\n", lower_cutoff, upper_cutoff))
    cat(sprintf("Retained %d out of %d points (%.1f%%)\n",
                length(filtered_slope_vector), length(slope_vector),
                100 * length(filtered_slope_vector) / length(slope_vector)))

    # patch_title = paste0("Possible Facets after KDE Filter - Building ",
    #                      building$building_id)
    # plot_raster2(patch_slope_raster,
    #             building,
    #             # xmin, xmax,
    #             # ymin, ymax,
    #             title = patch_title)
  }

  patch_slope_raster
}

