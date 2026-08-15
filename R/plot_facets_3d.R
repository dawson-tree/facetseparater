#' @import terra
#' @import rgl
#' @importFrom htmlwidgets saveWidget
#'
#' @export
plot_facets_3d <- function(results_obj,
                                         raster,
                                         building,
                                         thresh = 0.1,
                                         grid_res = 30,
                                         snapshot = FALSE,
                                         snapshot_file = "building_plot.png",
                                         save_html = FALSE,
                                         html_file = "building_plot.html") {
  building <- terra::vect(building)

  for (facet in results_obj$results_list) {
    # --- 1. pull result entry ---
    res_entry <- facet
    if(is.null(res_entry)) stop("No results for this building")
    plane_coeff <- res_entry$plane_coeff
    if(is.null(plane_coeff)) stop("No plane_coeff for this building")
    inliers_saved <- res_entry$inliers

    # convert saved inliers to data.frame if matrix
    if(is.matrix(inliers_saved)) {
      inliers_df <- as.data.frame(inliers_saved)
      colnames(inliers_df) <- c("x","y","z")
    } else {
      inliers_df <- as.data.frame(inliers_saved)
      # ensure names
      if(!all(c("x","y","z") %in% names(inliers_df))) {
        colnames(inliers_df)[1:3] <- c("x","y","z")
      }
    }

    # --- 2. re-extract all points from raster for that building polygon ---
    bldg_poly <- building
    # [values(buildings) == building_id, , drop = FALSE]
    # if(nrow(bldg_poly) == 0) stop("No polygon found for building_id ", building_id)

    # ex <- terra::extract(raster, vect(bldg_poly), cells = TRUE, xy = TRUE)
    ex <- terra::extract(raster, bldg_poly, cells = TRUE, xy = TRUE)
    ex <- as.data.frame(ex)
    ex <- na.omit(ex)
    # find elevation column (exclude ID, cell, x, y)
    exclude_cols <- c("ID","cell","x","y")
    z_col <- setdiff(names(ex), exclude_cols)[1]
    pts_all <- data.frame(x = ex$x, y = ex$y, z = ex[[z_col]])

    # --- 3. robust matching: create keys with rounding to avoid FP mismatch ---
    # choose rounding digits consistent with raster resolution; default 3 decimals
    round_digits <- 3
    pts_all$key <- paste0(round(pts_all$x, round_digits), "_", round(pts_all$y, round_digits), "_", round(pts_all$z, round_digits))
    inliers_df$key <- paste0(round(inliers_df$x, round_digits), "_", round(inliers_df$y, round_digits), "_", round(inliers_df$z, round_digits))

    # non-inliers = pts_all \ inliers
    non_inliers <- pts_all %>% filter(!key %in% inliers_df$key)
    # if non_inliers is empty, relax match by only x,y (not z)
    if(nrow(non_inliers) == 0) {
      pts_all$key_xy <- paste0(round(pts_all$x, round_digits), "_", round(pts_all$y, round_digits))
      inliers_df$key_xy <- paste0(round(inliers_df$x, round_digits), "_", round(inliers_df$y, round_digits))
      non_inliers <- pts_all %>% filter(!key_xy %in% inliers_df$key_xy)
    }

    # --- 4. prepare plane grid (over extent of inliers) ---
    x_seq <- seq(min(inliers_df$x), max(inliers_df$x), length.out = grid_res)
    y_seq <- seq(min(inliers_df$y), max(inliers_df$y), length.out = grid_res)
    grid <- expand.grid(x = x_seq, y = y_seq)
    # plane: z = intercept + coef_x * x + coef_y * y  (we used lm)
    grid$z <- plane_coeff["(Intercept)"] + plane_coeff["x"] * grid$x + plane_coeff["y"] * grid$y

    # convert to matrices for surface3d (matrix expects byrow = FALSE; dim must match)
    z_mat <- matrix(grid$z, nrow = grid_res, ncol = grid_res, byrow = FALSE)
    x_mat <- matrix(grid$x, nrow = grid_res, ncol = grid_res, byrow = FALSE)
    y_mat <- matrix(grid$y, nrow = grid_res, ncol = grid_res, byrow = FALSE)

    # --- 5. rgl plotting ---
    rgl::open3d()
    rgl::bg3d("white")
    # plot non-inliers first (background)
    if(nrow(non_inliers) > 0) {
      rgl::points3d(non_inliers$x, non_inliers$y, non_inliers$z, col = "gray70", size = 3)
    }
    # plot inliers
    rgl::points3d(inliers_df$x, inliers_df$y, inliers_df$z, col = "red", size = 6)
    # plane
    rgl::surface3d(x = x_seq, y = y_seq, z = z_mat, color = "skyblue", alpha = 0.5, front = "fill")

    # add axes and title
    rgl::axes3d(edges = c("x--", "y+-", "z--"))
    rgl::title3d(main = paste0("Facet ", facet$facet_no,
                          "  |  slope: ", round(res_entry$slope_deg,2), "o",
                          "  |  inliers: ", res_entry$n_inliers),
            xlab = "x", ylab = "y", zlab = "z")

  }

  # optional: save PNG snapshot
  if(isTRUE(snapshot)) {
    # small pause to let rgl render
    Sys.sleep(0.2)
    rgl::rgl.snapshot(snapshot_file)
    message("Saved snapshot to: ", snapshot_file)
  }

  # optional: save HTML interactive widget
  if(isTRUE(save_html)) {
    wid <- rgl::rglwidget()
    htmlwidgets::saveWidget(wid, html_file, selfcontained = TRUE)
    message("Saved interactive html to: ", html_file)
  }

  # return invisibly the plotted objects for downstream use
  invisible(list(inliers = inliers_df, non_inliers = non_inliers, plane_grid = grid))
}

