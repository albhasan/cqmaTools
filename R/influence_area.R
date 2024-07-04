#' @title Build a vector from the origin to both the min and max values
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description 
#' Build a vector from the minimum to the maximum value ensuring
#' that origin is a value in the returned vector.
#'
#' @param o   a numeric(1). The origin.
#' @param min a numeric(1). The mininum value.
#' @param max a numeric(1). The maximum value.
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
#' @param origin_lon,origin_lat a numeric. Coordinate of origin.
#' @param min_lon,max_lon  a numeric. Grid's mininum and maximum longitude 
#'   value.
#' @param min_lat,max_lat  a numeric. Grid's mininum and maximum latitudes 
#'   values.
#' @param grid_resolution A numeric. Grid's resolution.
#' @param crs A numeric. Coordinate reference system (EPSG).
#'
#' @return an sf object (polygon).
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
#' @param files a character. Paths to trajectory files (i.e. HYSPLIT files).
#' @param hs_skip a numeric. Number of lines to skip from each file.
#' @param hs_cnames a character. Names of the columns in each file.
#' @param hs_clon,hs_clat a character. Names of the longitude and latitude 
#'   columns in the files.
#' @param crs a numeric. EPSG code used for both the grid and the trajectories.
#' @param min_lon,max_lon a numeric. Grid's mininum and maximum longitude
#'   values.
#' @param min_lat,max_lat  a numeric(1). Grid's mininum and maximum latitude
#'   values.
#' @param grid_resolution A numeric. Grid's resolution.
#' @param traj_min_height,traj_max_height a numeric(1). Remove trajectories 
#'   which, at some vertex, fall ouside this range.
#' @param vert_min_height,ver_max_height Remove vertices from trajectories
#'   falling outsize this range.
#'
#' @return an terra object. The raster values correspond to the number of 
#'   trajectory vertices in each cell.
#'
#' @export
#'
compute_frequency_grid <- function(files, hs_skip = 7,
                                   hs_cnames = HYSPLIT.COLNAMES,
                                   hs_clon = "lon", hs_clat = "lat",
                                   crs = 4326,
                                   min_lon = -80, max_lon = -30, 
                                   min_lat = -40, max_lat = 10,
                                   grid_resolution = 2, 
                                   traj_min_height = -Inf, 
                                   traj_max_height = Inf,
                                   vert_min_height = -Inf,
                                   vert_max_height = Inf) {

    # Read trajectory files into a data frames
    hysplit_df_ls <- cqmaTools::files2df(
        files = files,
        header = FALSE,
        hs_skip = hs_skip,
        hs_cnames = hs_cnames
    )

    # Filter data frames (trajectories) by height.
    h_filter <- vapply(hysplit_df_ls, function(x){
        return(all(
            all(x[["height"]] > traj_min_height),
            all(x[["height"]] < traj_max_height)
        ))
    }, logical(1))

    # Bind data frames into one.
    hysplit_df <- do.call(rbind, hysplit_df_ls[h_filter])

    # Filter trajectories' vertices by height.
    hysplit_df <- hysplit_df[hysplit_df[["height"]] > vert_min_height &
                             hysplit_df[["height"]] < vert_max_height,]

    # Build a grid. NOTE: Assume the first vertex is the grid's origin.
    aoi_grid <- build_grid(
        origin_x = hysplit_df[1, hs_clon],
        origin_y = hysplit_df[1, hs_clat],
        min_lon, max_lon, min_lat, 
        max_lat, grid_resolution, crs = crs
    )

  # Build a sf object (point) using the trajectories' vertices.
    hysplit_sf <- sf::st_as_sf(
        x = hysplit_df, 
        coords = c(hs_clon, hs_clat), 
        crs = crs
    )

    # Cross the grid and trajetories' vertices.

    # NOTE: s2 is slow at running st_intersection.
    suppressMessages({
        s2 <- sf::sf_use_s2()
        sf::sf_use_s2(FALSE)
        hysplit_sf <- sf::st_intersection(x = hysplit_sf, y = aoi_grid)
        sf::sf_use_s2(s2)
    })

    grid_traj_freq <- as.data.frame(table(
        sf::st_drop_geometry(hysplit_sf)[["gid"]]
    ))

    colnames(grid_traj_freq) <- c("gid", "freq")
    aoi_grid <- merge(
        x = aoi_grid, 
        y = grid_traj_freq, 
        by = "gid", 
        all.x = TRUE
    )

    # Cast vector grid to raster.
    template <- terra::rast(
        terra::vect(aoi_grid), 
        resolution = grid_resolution
    )
    aoi_grid_r <- terra::rasterize(
        terra::vect(aoi_grid), 
        y = template, 
        field = "freq"
    )

    return(aoi_grid_r)
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
  cv_pol <- st_sfc(cv_pol, crs = crs(r))
  cv_pol <- st_sf(id = 1, cv_pol)
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

