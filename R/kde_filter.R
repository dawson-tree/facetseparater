#' Filter raster pixels using kernel density estimation
#'
#' Use kernel density estimation to estimate the distribution of the slope of
#'     rooftop pixels. Then only pixels in the primary distribution are kept to
#'     filter out the pixels that are most likely to contain facet
#'     edges. An additional processing step removes single pixel-wide bridges
#'     connecting multiple groups of valid pixels. At this point, the raster is
#'     divided into separate roof facets. A summary of the kernel density
#'     estimation results is printed as well.
#'
#' @param building An object of the classes 'sf::sf' and 'data.frame' that was
#'     created using the workflow from the package 'rasterpolygonizer' that
#'     has one observation representing the interior polygon of one building.
#' @param full_slope_raster A raster of the class 'terra::SpatRaster' that shows
#'     the slope value for each pixel, as calculated by the 'terra:terrain'
#'     function.
#' @param adjust The 'adjust' argument given to the 'density' function, or the
#'     adjustment to the smoothing bandwidth used for kernel density estimation.
#'     More detailed information can be found in the 'density'function
#'     documentation.
#' @param n The 'n' argument given to the 'density' function, or the number of
#'     points at which the density is estimated when performing kernel density
#'     estimation. More information can be found in the 'density' function
#'     documentation.
#' @param quiet Logical argument indicating whether summary messages should be
#'     printed.
#'
#' @return A 'terra::SpatRaster' object where the value of each pixel indicates
#'     the roof facet (if any) that it is a part of.
#'
#' @importFrom terra patches
kde_filter <- function(building,
                       slope_raster,
                       adjust = 1,
                       n = 2048,
                       quiet = FALSE) {

  # slope_raster <- crop(full_slope_raster, building, mask = TRUE)

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

  # Erosion function

  eroded_slope_raster <- erode(clamped_slope_raster)

  patch_slope_raster <- terra::patches(eroded_slope_raster, directions = 4) # ????

  if (!quiet) {

    # Print KDE summary
    cat(sprintf("Main Peak at: %.2f\n", main_peak_x))
    cat(sprintf("Boundaries: [%.2f, %.2f]\n", lower_cutoff, upper_cutoff))
    cat(sprintf("Retained %d out of %d points (%.1f%%)\n",
                length(filtered_slope_vector), length(slope_vector),
                100 * length(filtered_slope_vector) / length(slope_vector)))

  }

  patch_slope_raster
}

