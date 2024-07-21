#' Aggregate trajectories' vertices using a grid
#'
#' @description 
#' Compute the number of trajectories' vertices for each cell in the grid.
#'
#' @param files a character. Paths to trajectory files (i.e. HYSPLIT files).
#' @param grid_sf an sf object (polygons). The grid used to aggregate
#'   trajectories. 
#' @param skip a numeric. Number of lines to skip from each file.
#' @param from_row,to_row a numeric(1). Use a subset of rows from each data
#'   frame.
#' @param cnames a character. Names of the columns in each file.
#' @param clon,clat,cheight a character. Names of the longitude, latitude, and
#'   height columns in `cnames` 
#' @param crs a numeric. EPSG code used for the trajectories.
#' @param traj_min_lon,traj_max_lon,traj_min_lat,traj_max_lat,traj_min_height,traj_max_height a numeric(1). Remove trajectories which, at some vertex, fall outside of these ranges.
#' @param vert_min_lon,vert_max_lon,vert_min_lat,vert_max_lat,vert_min_height,vert_max_height a numeric(1). Remove vertices from trajectories falling outside of these ranges.
#' @param min_per_vert_in_hrange a double(1). Minimum percentage of vertices
#'   inside height range for a trajectory to be valid.
#'
#' @seealso [build_grid] for building grids.
#'
#' @return an sf object. The given grid_sf object with additional attributes ("freq".
#'
#' @export
#'
compute_frequency_grid <- function(files, 
                                   grid_sf,
                                   skip = 7,
                                   from_row = 1,
                                   to_row = Inf,
                                   cnames = HYSPLIT.COLNAMES,
                                   clon = "lon",
                                   clat = "lat",
                                   cheight = "height",
                                   crs = 4326,
                                   traj_min_lon = -Inf,
                                   traj_max_lon = Inf,
                                   traj_min_lat = -Inf,
                                   traj_max_lat = Inf,
                                   traj_min_height = -Inf,
                                   traj_max_height = Inf,
                                   vert_min_lon = -Inf,
                                   vert_max_lon = Inf,
                                   vert_min_lat = -Inf,
                                   vert_max_lat = Inf,
                                   vert_min_height = -Inf,
                                   vert_max_height = Inf,
                                   min_per_vert_in_hrange = 0) {

    stopifnot("No files given!" = length(files) > 0)
    stopifnot("`height`, `lon`, or `lat` columns not found in data frame!" = 
              c(cheight, clon, clat) %in% cnames)
    stopifnot("Invalid number of files" = length(files) > 0)
    stopifnot("Expected an sf object for a grid!" = 
        inherits(grid_sf, what = "sf"))
    stopifnot("Expected a grid of type POLYGON" = 
        as.character(sf::st_geometry_type(grid_sf, by_geometry = FALSE)) %in% 
            "POLYGON")
    stopifnot("Id column `gid` not found in grid!" =
        "gid" %in% colnames(grid_sf))
    stopifnot("Invalid `min_per_vert_in_hrange`!" = 
        min_per_vert_in_hrange >= 0 & min_per_vert_in_hrange <= 1 &
        length(min_per_vert_in_hrange) == 1)

    # Read trajectory files into a data frames
    data_df_ls <- files2df(
        files = files,
        header = FALSE,
        skip = skip,
        cnames = cnames
    )

    # Filter trajectories.
    traj_ls <- filter_traj(data_df_ls, from_row = from_row, to_row = to_row,
        clon = clon, clat = clat, cheight = cheight,
        traj_min_lon = traj_min_lon, traj_max_lon = traj_max_lon,
        traj_min_lat = traj_min_lat, traj_max_lat = traj_max_lat,
        traj_min_height = traj_min_height, traj_max_height = traj_max_height,
        vert_min_lon = vert_min_lon, vert_max_lon = vert_max_lon,
        vert_min_lat = vert_min_lat, vert_max_lat = vert_max_lat,
        vert_min_height = vert_min_height, vert_max_height = vert_max_height,
        min_per_vert_in_hrange = min_per_vert_in_hrange)

    # Bind data frames into one.
    traj_df <- do.call(rbind, traj_ls)
    if (nrow(traj_df) == 0) {
        warning("Empty data frame!")
        return(NA)
    }

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
        sf::st_drop_geometry(traj_sf)[["gid"]]
    ))

    colnames(grid_traj_freq) <- c("gid", "freq")
    grid_sf <- merge(
        x = grid_sf, 
        y = grid_traj_freq, 
        by = "gid", 
        all.x = TRUE
    )

    return(grid_sf)

}



#' @title  Build a convex hull around a raster
#' @name raster2convexhull
#' @author Alber Sánchez, \email{alber.ipia@@inpe.br}
#'
#' @description Build a convex hull round the pixels in the given raster which
#' are not NULL.
#' 
#' @param r A raster (terra).
#'
#' @return A polygon (sf).
#'
raster2convexhull <- function(r) {
  xy <- sf::st_coordinates(sf::st_as_sf(terra::as.points(r)))
  chrows <- grDevices::chull(x = xy[,"X"], y = xy[,"Y"])
  chrows <- c(chrows, chrows[1])
  cv_pol <- sf::st_polygon(x = list(xy[chrows, ]))
  cv_pol <- sf::st_sfc(cv_pol, crs = terra::crs(r))
  cv_pol <- sf::st_sf(id = 1, cv_pol)
  return(cv_pol)
}



#' @title Smooth raster using NA neighbors
#' @name smooth_neigh_na
#' @author Alber Sánchez, \email{alber.ipia@@inpe.br}
#' 
#' @description Transform the cells into NA depending on the number of NA 
#' neighbors.
#'
#' @param r A raster (terra).
#' @param w An integer. The window size.
#' @param threshold An integer. 
#'
#' @return A raster (terra).
#'
smooth_neigh_na <- function(r, w = 3, threshold = 7) {
  nacount <- terra::focal(
    x = r < 0 | is.na(r), 
    w = w, 
    fun = "sum"
  )
  r <- terra::mask(r, mask = nacount < threshold)
  return(r)
}

