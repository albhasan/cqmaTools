#' Aggregate trajectories' vertices using a grid
#'
#' @description
#' Compute the number of trajectories' vertices for each cell in the grid.
#'
#' @param traj_df a data frame. Data representing a trajectory.
#' @param grid_sf an sf object (polygons). The grid used to aggregate
#'   trajectories.
#' @param clon,clat,cheight a character(1). Names of the longitude, latitude,
#'   and height columns in `traj_df`.
#' @param crs a numeric. EPSG code used for the trajectory data in `traj_df`.
#' @param grid_id a character(1). Name of the column in `grid_sf` which
#'   identifies each cell in the grid.
#' @param densify_n an integer. Increment the number of vertices in each of
#' the segments in the given trajectory (see 'smoothr::densify').
#'
#' @seealso [build_grid] for building grids.
#'
#' @return an sf object. The given grid_sf object with additional attributes
#' ("freq").
#'
#' @export
#'
compute_frequency_grid <- function(traj_df,
                                   grid_sf,
                                   clon = "lon",
                                   clat = "lat",
                                   cheight = "height",
                                   grid_id = "grid_id",
                                   crs = sf::st_crs(grid_sf),
                                   densify_n = 1) {

  stopifnot("`traj_df` must be a data frame!" = is.data.frame(traj_df))
  stopifnot("`height`, `lon`, or `lat` columns not found in data frame!" =
              c(cheight, clon, clat) %in% colnames(traj_df))
  stopifnot(
    "Expected an sf object for a grid!" = inherits(grid_sf, what = "sf")
  )
  stopifnot(
    "Expected a grid of type POLYGON" =
      as.character(sf::st_geometry_type(grid_sf, by_geometry = FALSE))
      %in% "POLYGON"
  )
  stopifnot(
    "Id column `grid_id` not found in grid!" = grid_id %in% colnames(grid_sf)
  )

  # Build a sf object (point) using the trajectories' vertices.
  points_sf <- sf::st_as_sf(
    x = traj_df,
    coords = c(clon, clat),
    crs = crs
  )
  # Increase the number of vertices in trajectories' segments.
  if (densify_n > 1) {
    lines_sf <- sf::st_linestring(sf::st_coordinates(points_sf))
    lines_sf <- sf::st_as_sfc(list(lines_sf), crs = sf::st_crs(points_sf))
    lines_sf <- smoothr::densify(lines_sf, n = densify_n)
    points_sf <- sf::st_as_sf(sf::st_cast(lines_sf, "POINT"))
  }
  # Cross the grid and trajetories' vertices.
  # NOTE: s2 is slow at running st_intersection.
  s2 <- sf::sf_use_s2()
  suppressMessages({
    sf::sf_use_s2(FALSE)
  })
  sf::st_agr(points_sf) <- sf::st_agr(grid_sf) <- "constant"
  points_sf <- sf::st_intersection(x = points_sf, y = grid_sf)
  suppressMessages({
    sf::sf_use_s2(s2)
  })

  grid_traj_freq <- as.data.frame(table(
    sf::st_drop_geometry(points_sf)[[grid_id]]
  ))

  colnames(grid_traj_freq) <- c(grid_id, "freq")
  grid_sf <- merge(
    x = grid_sf,
    y = grid_traj_freq,
    by = grid_id,
    all.x = TRUE
  )

  return(grid_sf)

}

#' Plot influence area
#'
#' @description
#' Plot the given influence area (a raster).
#'
#' @param r a raster. The area of influence.
#' @param r_range a numeric(2). The range of values used during plot.
#' @param r_col a color palette for mapping. See [terra::map.pal].
#' @param x_range,y_range a numeric(2). The plot range in x and y.
#' @param add_countries a logical(1). Should countries' borderds be added?
#' @param ctr_color a character(1). Color name used to plot the countries.
#' @param ctr_lwd a numeric(1). Line width used to plot the countries.
#' @param add_states a logical(1). Should we add the states to the plot?
#' @param stt_color a character(1). Color name for the Brazilian states.
#' @param stt_lwd a numeric(1). Line width for the Brazilian states.
#' @param plot_title a character(1). Title of the plot.
#' @param save_plots either a logical(1) or a character(1). Should the plot be
#'   saved to files? If so, a path to a directory should be provided,
#'   otherwise, the current directory is used. The file name is taken from
#'   `plot_title`.
#' @param plot_width,plot_height a numeric(1). Size of the plot.
#'
#' @return a character. The path to the files created.
#'
#' @export
#'
plot_influence_area <- function(r,
                                r_range = range(r[]),
                                r_col = terra::map.pal("viridis", 100),
                                x_range = c(-180, 180),
                                y_range = c(-90, 90),
                                add_countries = TRUE,
                                ctr_color = "black",
                                ctr_lwd = 2.0,
                                add_states = TRUE,
                                stt_color = "gray",
                                stt_lwd = 1.0,
                                plot_title = "",
                                save_plots = FALSE,
                                plot_width = 480,
                                plot_height = 480) {

  stopifnot("Expected a `terra::rast` object!" =
              inherits(r, what = "SpatRaster"))

  plot_fname <- NA
  plot2file <- FALSE
  if (save_plots == TRUE)
    save_plots <- getwd()

  if (is.character(save_plots))
    if (dir.exists(save_plots))
      plot2file <- TRUE

  if (plot2file) {
    plot_fname <-
      file.path(
        save_plots,
        paste0(
          "plot_aoi_",
          gsub(pattern = "[.]", replacement = "_", plot_title),
          ".png"
        )
      )
    grDevices::png(
      filename = plot_fname,
      width = plot_width,
      height = plot_height
    )
  }

  terra::plot(
    x = r,
    range = r_range,
    xlim = x_range,
    ylim = y_range,
    main = plot_title,
    col = r_col
  )

  if (add_states) {
    states_sf <- sf::st_transform(states_sf, crs = terra::crs(r))
    states_sf <- states_sf[[get_geom_colname(states_sf)]]
    plot(states_sf, border = stt_color, lwd = stt_lwd, type = "l", add = TRUE)
  }

  if (add_countries) {
    countries_sf <- sf::st_transform(countries_sf, crs = terra::crs(r))
    countries_sf <- countries_sf[[get_geom_colname(countries_sf)]]
    plot(
      countries_sf,
      border = ctr_color,
      lwd = ctr_lwd,
      type = "l",
      add = TRUE,
      xlim = x_range,
      ylim = y_range
    )
  }

  if (plot2file)
    grDevices::dev.off()

  invisible(plot_fname)

}

