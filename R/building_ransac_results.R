#' Get RANSAC algorithm results for multiple roof facets
#'
#' Perform the RANSAC algorithm to estimate the slope and aspect of multiple
#'     roof facets from one building.
#'
#' @param facets A 'terra::SpatRaster' object that represents the various facets
#'     on a building rooftop. This type of object is the output of the
#'     'kde_filter' function.
#' @param raster An elevation raster of the class 'terra::SpatRaster'.
#' @param building_id The 'building_id' for the specific building rooftop to be
#'     analyzed.
#' @param n_iter Number of RANSAC iterations for each facet.
#' @param thresh Residual threshold for inlier classification in meters.
#' @param min_inliers Minimum number of inliers required to accept a plane.
#' @param quiet Logical argument indicating whether progress messages should be
#'     printed.
#'
#' @return A list containing the following:
#' \describe{
#'   \item{results_list}{A list of lists with detailed results for each roof
#'       facet.}
#'   \item{summary_table}{Data frame summarizing slope, aspect, snow depth,
#'       inliers, and stability metrics for each roof facet.}
#'   \item{facet_polys}{A terra::SpatVector of polygons representing each of the
#'       final roof facets}
#' }
building_ransac_results <- function(facets,
                                    raster,
                                    building_id,
                                    n_iter = 500L,
                                    thresh = 0.1,
                                    min_inliers = 10L,
                                    quiet = FALSE) {
  # --- Input validation ---
  if (!inherits(raster, "SpatRaster")) {
    stop("`raster` must be a terra::SpatRaster.")
  }

  if (!inherits(facets, "SpatVector")) {
    stop("`facets` must be a terra::SpatVector.")
  }

  results <- list()

  for (i in seq_len(terra::nrow(facets))) {
    bldg <- facets[i, ]
    id <- i

    # --- Extract raster values ---
    ex <- terra::extract(raster, bldg, cells = TRUE, xy = TRUE)
    v <- stats::na.omit(ex)

    if (nrow(v) < min_inliers) {
      if (!quiet) message("Facet ", id, ": Not enough points")
      next
    }

    exclude_cols <- c("ID", "cell", "x", "y")
    z_col <- setdiff(names(v), exclude_cols)[1]

    xyz <- cbind(v$x, v$y, v[[z_col]])
    colnames(xyz) <- c("x", "y", "z")

    best_inliers <- NULL
    best_plane <- NULL

    candidate_records <- vector("list", n_iter)
    n_candidates <- 0L

    for (j in seq_len(n_iter)) {
      idx <- sample.int(nrow(xyz), 3L)
      pts <- xyz[idx, , drop = FALSE]

      fit <- tryCatch(
        stats::lm(z ~ x + y, data = as.data.frame(pts)),
        error = function(e) NULL
      )

      if (is.null(fit)) next

      beta_tmp <- stats::coef(fit)
      if (any(is.na(beta_tmp))) next

      z_pred_tmp <- beta_tmp[1] +
        beta_tmp[2] * xyz[, 1] +
        beta_tmp[3] * xyz[, 2]

      residuals_tmp <- abs(xyz[, 3] - z_pred_tmp)

      valid_idx <- which(!is.na(residuals_tmp))
      residuals_tmp <- residuals_tmp[valid_idx]
      xyz_valid <- xyz[valid_idx, , drop = FALSE]

      inliers <- xyz_valid[residuals_tmp < thresh, , drop = FALSE]

      if (nrow(inliers) < min_inliers) next

      # --- Candidate qualifies ---
      a_tmp <- beta_tmp["x"]
      b_tmp <- beta_tmp["y"]

      slope_tmp <- atan(sqrt(a_tmp^2 + b_tmp^2)) * 180 / pi

      n_candidates <- n_candidates + 1L
      candidate_records[[n_candidates]] <- list(
        n_inliers = nrow(inliers),
        slope_deg = slope_tmp
      )

      if (is.null(best_inliers) ||
        nrow(inliers) > nrow(best_inliers)) {
        best_inliers <- inliers
        best_plane <- beta_tmp
      }
    }

    if (is.null(best_plane)) {
      if (!quiet) message("Facet ", id, ": No valid plane found")
      next
    }

    candidate_records <- candidate_records[seq_len(n_candidates)]

    slope_spread <- NA_real_
    top5_count <- NA_integer_

    if (n_candidates >= 1L) {
      inlier_counts <- vapply(
        candidate_records,
        `[[`,
        numeric(1),
        "n_inliers"
      )

      top_n <- max(1L, ceiling(n_candidates * 0.05))
      top_idx <- order(inlier_counts, decreasing = TRUE)[seq_len(top_n)]

      top_slopes <- vapply(
        candidate_records[top_idx],
        `[[`,
        numeric(1),
        "slope_deg"
      )

      top5_count <- length(top_slopes)

      if (top5_count >= 2L) {
        slope_spread <- diff(
          stats::quantile(top_slopes, probs = c(0.025, 0.975))
        )
      }
    }

    # --- Final slope + aspect ---
    a <- best_plane["x"]
    b <- best_plane["y"]

    slope_deg <- atan(sqrt(a^2 + b^2)) * 180 / pi
    aspect_deg <- (atan2(-a, -b) * 180 / pi) %% 360

    results[[as.character(id)]] <- list(
      facet_no      = id,
      plane_coeff   = best_plane,
      slope_deg     = slope_deg,
      aspect_deg    = aspect_deg,
      n_inliers     = nrow(best_inliers),
      slope_spread  = slope_spread,
      top5_count    = top5_count,
      inliers       = best_inliers
    )

    if (!quiet) {
      message(
        "Facet ", id, " Processed",
        " | Slope: ", round(slope_deg, 2), "\u00B0",
        " | Inliers: ", nrow(best_inliers),
        " | Top 5% Spread: ", round(slope_spread, 2), "\u00B0"
      )
    }
  }

  final_facet_polys <- facets[as.numeric(names(results)), ]

  names(results) <- seq_along(results)

  for (i in seq_along(results)) {
    results[[i]][["facet_no"]] <- i
  }

  summary_table <- do.call(
    rbind,
    lapply(results, function(x) {
      data.frame(
        facet_no = x$facet_no,
        slope_deg = x$slope_deg,
        aspect_deg = x$aspect_deg,
        n_inliers = x$n_inliers,
        slope_range = x$slope_spread,
        top_5_percent_count = x$top5_count,
        building_id = building_id,
        stringsAsFactors = FALSE
      )
    })
  )

  rownames(summary_table) <- NULL

  building_results <- list(
    results_list = results,
    summary_table = summary_table,
    facet_polys = final_facet_polys
  )

  building_results
}
