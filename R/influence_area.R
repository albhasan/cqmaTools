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
                                   crs = sf::st_crs(grid_sf)) {

    stopifnot("`traj_df` must be a data frame!" = is.data.frame(traj_df))
    stopifnot("`height`, `lon`, or `lat` columns not found in data frame!" = 
              c(cheight, clon, clat) %in% colnames(traj_df))
    stopifnot("Expected an sf object for a grid!" = 
        inherits(grid_sf, what = "sf"))
    stopifnot("Expected a grid of type POLYGON" = 
        as.character(sf::st_geometry_type(grid_sf, by_geometry = FALSE)) %in% 
            "POLYGON")
    stopifnot("Id column `grid_id` not found in grid!" =
        grid_id %in% colnames(grid_sf))

    # Build a sf object (point) using the trajectories' vertices.
    traj_sf <- sf::st_as_sf(
        x = traj_df,
        coords = c(clon, clat), 
        crs = crs
    )

    # Cross the grid and trajetories' vertices.
    # NOTE: s2 is slow at running st_intersection.
    s2 <- sf::sf_use_s2()
    suppressMessages({ sf::sf_use_s2(FALSE) })
    sf::st_agr(traj_sf) <- sf::st_agr(grid_sf) <- "constant"
    traj_sf <- sf::st_intersection(x = traj_sf, y = grid_sf)
    suppressMessages({ sf::sf_use_s2(s2) })

    grid_traj_freq <- as.data.frame(table(
        sf::st_drop_geometry(traj_sf)[[grid_id]]
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

