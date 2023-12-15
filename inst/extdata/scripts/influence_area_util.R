#' @title Build a vector from the origin to both the min and max values
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Build a vector from the minimum to the maximum value ensuring
#' that origin is a value in the returned vector.
#'
#' @param o A numeric. The origin.
#' @param min A numeric. The mininum value.
#' @param max A numeric. The maximum value.
#'
#' @return    A numeric.
#'
.grid_helper <- function(o, min, max, res) {
  sort(c(seq(from = o, to = max, by = res), 
         seq(from = o, to = min, by = -res)[-1]))
}



#' @title Build a grid
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Build a grid that includes the given origin as a vertex.
#'
#' @param origin_lon A numeric. Coordinate of the origin.
#' @param origin_lat A numeric. Coordinate of the origin.
#' @param min_lon  A numeric. Grid's mininum longitude value.
#' @param max_lon  A numeric. Grid's maximum longitude value.
#' @param min_lat  A numeric. Grid's mininum latitude value.
#' @param max_lat  A numeric. Grid's maximum latitude value.
#' @param grid_resolution A numeric. Grid's resolution.
#' @param crs A numeric. Coordinate reference system (EPSG).
#'
#' @return value An sf object (polygon).
#'
#' @export
#'
build_grid <- function(origin_x, origin_y, min_lon, max_lon, min_lat, 
                       max_lat, grid_resolution, crs) {

  lon_grid <- .grid_helper(o = origin_x, min = min_lon, max = max_lon,
                           res = grid_resolution)
  lat_grid <- .grid_helper(o = origin_y, min = min_lat, max = max_lat,
                           res = grid_resolution)
  aoi_grid <-
    sf::st_as_sf(sf::st_make_grid(
      x = sf::st_bbox(c(xmin = min(lon_grid), xmax = max(lon_grid),
                        ymin = min(lat_grid), ymax = max(lat_grid)),
                      crs = sf::st_crs(crs)),
      cellsize = grid_resolution
    ))
  aoi_grid["gid"] <- seq(nrow(aoi_grid))
  return(aoi_grid)
}



#' @title Aggregate trajectory points using a grid
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Compute the number of trajectory vertices for each cell in the
#' grid.
#'
#' @param files A character. Paths to trajectory files (i.e. HYSPLIT files).
#' @param hs_skip A numeric. Number of lines to skip from each file.
#' @param hs_cnames A character. Names of the columns in each file.
#' @param hs_clon A character. Name of the longitude column in the files.
#' @param hs_clat A character. Name of the latitude column in the files.
#' @param crs A numeric. EPSG code used for both the grid and the trajectories.
#' @param min_lon  A numeric. Grid's mininum longitude value.
#' @param max_lon  A numeric. Grid's maximum longitude value.
#' @param min_lat  A numeric. Grid's mininum latitude value.
#' @param max_lat  A numeric. Grid's maximum latitude value.
#' @param grid_resolution A numeric. Grid's resolution.
#'
#' @return An terra object (raster). The raster values correspond to the number
#' of trajectory vertices in each cell.
#'
#' @export
#'
compute_frequency_grid <- function(files, hs_skip = 7,
                                   hs_cnames = HYSPLIT.COLNAMES,
                                   hs_clon = "lon", hs_clat = "lat",
                                   crs = 4326,
                                   min_lon = -80, max_lon = -30, 
                                   min_lat = -40, max_lat = 10,
                                   grid_resolution = 2) {

  # Read trajectory files into a dataframe.
  hysplit_df <-
    do.call(
      rbind,
      cqmaTools::files2df(
        file.vec = files,
        header = FALSE,
        skip = hs_skip,
        cnames = hs_cnames
      )
    )

  # Build a grid. NOTE: Assume the first vertex is the grid's origin.
  aoi_grid <- build_grid(origin_x = hysplit_df[1, hs_clon],
                         origin_y = hysplit_df[1, hs_clat],
                         min_lon, max_lon, min_lat, 
                         max_lat, grid_resolution, crs = crs)

  # Build a sf object (point) using the trajectories' vertices.
  hysplit_sf <-
    sf::st_as_sf(x = hysplit_df, coords = c(hs_clon, hs_clat), crs = crs)

  # Cross the grid and trajetories' vertices.
  # NOTE: s2 is slow at running st_intersection.
  suppressMessages({
    s2 <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)
    hysplit_sf <- sf::st_intersection(x = hysplit_sf, y = aoi_grid)
    sf::sf_use_s2(s2)
  })
  grid_traj_freq <-
    as.data.frame(table(
      sf::st_drop_geometry(hysplit_sf)[["gid"]]
    ))
  colnames(grid_traj_freq) <- c("gid", "freq")
  aoi_grid <- merge(x = aoi_grid, y = grid_traj_freq, by = "gid", all.x = TRUE)

  # Cast vector grid to raster.
  template <- terra::rast(terra::vect(aoi_grid), resolution = grid_resolution)
  aoi_grid_r <- terra::rasterize(terra::vect(aoi_grid), 
                                 y = template, 
                                 field = "freq")

  return(aoi_grid_r)
}



#' @title Compute the influence area
#' @name compute_influence_area
#' @author Alber Sánchez, \email{alber.ipia@@inpe.br}
#'
#' @description Calculate the area of influence of the given trajectories of
#' air parcels. See references.
#'
#' @param files A character. Paths to trajectory files (i.e. Hysplit files).
#' @param min_lon  A numeric. Grid's mininum longitude value.
#' @param max_lon  A numeric. Grid's maximum longitude value.
#' @param min_lat  A numeric. Grid's mininum latitude value.
#' @param max_lat  A numeric. Grid's maximum latitude value.
#' @param grid_resolution A numeric. Grid's resolution (in degrees).
#' @param plot_aoi A logical. Should I plot the AOI?
#' @param plot_title A character. Title for the plot.
#'
#' @return A two-column matrix with coordinates of the area of influence.
#'
#' @references Cassol, H. L. G. et al. Determination of Region of Influence
#' Obtained by Aircraft Vertical Profiles Using the Density of Trajectories
#' from the HYSPLIT Model. Atmosphere 11, 1073 (2020).
#'
#' @export
#'
compute_influence_area <- function(files, hs_skip = 7, 
                                   hs_cnames = HYSPLIT.COLNAMES,
                                   crs = 4326,
                                   min_lon = -80, max_lon = -30, 
                                   min_lat = -40, max_lat = 10, 
                                   grid_resolution = 2, 
                                   plot_aoi = TRUE, 
                                   plot_title = "Area of Influence") {

  # Compute the number of trajectory vertices in each cell in the grid. 
  aoi_grid_r <-
    compute_frequency_grid(files = files, hs_skip = hs_skip, 
                           hs_cnames = hs_cnames, crs = crs, min_lon = min_lon, 
                           max_lon = max_lon, min_lat = min_lat, 
                           max_lat = max_lat,
                           grid_resolution = grid_resolution)

  # Filter
  aoi_grid_r <- terra::mask(aoi_grid_r, mask = aoi_grid_r >= 5.5)

  # Remove cells with certain NA neighbors.
  nacount <- terra::focal(x = aoi_grid_r < 0, w = 3, fun = "sum")
  aoi_grid_r <- terra::mask(aoi_grid_r, mask = nacount < 7)

  # Compute the logarithm.
  aoi_grid_r <- log(aoi_grid_r)

  # Build a convex hull.
  aoi_xy <- sf::st_coordinates(sf::st_as_sf(terra::as.points(aoi_grid_r)))
  chrows <- grDevices::chull(x = aoi_xy[,"X"], y = aoi_xy[,"Y"])
  chrows <- c(chrows, chrows[1])

  if (plot_aoi) {
    nbreaks <- 10
    intBreaks <- seq(2, 8, length.out = nbreaks)
    plot(aoi_grid_r, col = rev(heat.colors(nbreaks)),
         breaks = intBreaks, main = plot_title)
    lines(aoi_xy[chrows,], col = "red")
    maps::map("world", xlim = c(min_lon, max_lon),
              ylim = c(min_lat, max_lat), add = TRUE)
  }

  return(aoi_xy[chrows,])
}

