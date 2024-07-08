




#' @title Aggregate trajectory points using a grid
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Compute the number of trajectory vertices for each cell in the
#' grid.
#'
#' @param files a character. Paths to trajectory files (i.e. HYSPLIT files).
#' @param hs_skip a numeric. Number of lines to skip from each file.
#' @param hs_cnames a character. Names of the columns in each file.
#' @param hs_clon,hs_clat,hs_cheight a character. Names of the longitude,
#'   latitude, and height columns in `hs_cnames` 
#' @param crs a numeric. EPSG code used for both the grid and the trajectories.
#' @param min_lon,max_lon,min_lat,max_lat a numeric(1). Grid's mininum and 
#'   maximum values for longitude and latitude.
#' @param grid_resolution A numeric. Grid's resolution.

#' @param traj_min_lon,traj_max_lon,traj_min_lat,traj_max_lat,traj_min_height,traj_max_height a numeric(1). Remove trajectories which, at some vertex, fall ouside this ranges.
#' @param vert_min_lon,vert_max_lon,vert_min_lat,vert_max_lat,vert_min_height,vert_max_height a numeric(1). Remove vertices from trajectories falling outsize this ranges.
#'
#' @return a terra object. The raster values correspond to the number of
#'   trajectory vertices in each cell.
#'
#' @export
#'
compute_frequency_grid <- function(files, 
                                   hs_skip = 7,
                                   hs_cnames = HYSPLIT.COLNAMES,
                                   hs_clon = "lon",
                                   hs_clat = "lat",
                                   hs_cheight = "height",
                                   crs = 4326,
                                   min_lon = -80, 
                                   max_lon = -30, 
                                   min_lat = -40, 
                                   max_lat = 10,
                                   grid_resolution = 2, 
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
                                   vert_max_height = Inf
                                   ) {

    stopifnot("Height, lon, or lat columns not found in data frame!" = 
              c(hs_cheight, hs_clon, hs_clat) %in% hs_cnames)
    stopifnot("Invalid number of files" = length(files) > 0)

    # Read trajectory files into a data frames
    data_df_ls <- files2df(
        files = files,
        header = FALSE,
        skip = hs_skip,
        cnames = hs_cnames
    )

    # Filter data frames (trajectories) by height, longitude, and latitude.
    data_df_ls <- filter_data_frames(x = data_df_ls,
                                     cname = hs_cheight,
                                     min = traj_min_height,
                                     max = traj_max_height)
    if (length(data_df_ls) == 0) {
        warning("No trajectory meets the height filter!")
        return(NA)
    }

    data_df_ls <- filter_data_frames(x = data_df_ls,
                                     cname = hs_clon,
                                     min = traj_min_lon,
                                     max = traj_max_lon)
    if (length(data_df_ls) == 0) {
        warning("No trajectory meets the longitude filter!")
        return(NA)
    }

    data_df_ls <- filter_data_frames(x = data_df_ls,
                                     cname = hs_clat,
                                     min = traj_min_lat,
                                     max = traj_max_lat)
    if (length(data_df_ls) == 0) {
        warning("No trajectory meets the latitude filter!")
        return(NA)
    }

    # Bind data frames into one.
    hysplit_df <- do.call(rbind, data_df_ls)
    if (nrow(hysplit_df) == 0) {
        warning("Empty data frame!")
        return(NA)
    }

    # Filter trajectories' vertices by height, longitude, and latitude.
    if (!all(vert_min_height == -Inf, vert_max_height == Inf))
        hysplit_df <- hysplit_df[hysplit_df[[hs_cheight]] > vert_min_height &
                                 hysplit_df[[hs_cheight]] < vert_max_height,]
    if (nrow(hysplit_df) == 0) {
        warning("No trajectory vertex meets the height filter!")
        return(NA)
    }

    if (!all(vert_min_lon == -Inf, vert_max_lon == Inf))
        hysplit_df <- hysplit_df[hysplit_df[[hs_clon]] > vert_min_lon &
                                 hysplit_df[[hs_clon]] < vert_max_lon,]
    if (nrow(hysplit_df) == 0) {
        warning("No trajectory vertex meets the longitude filter!")
        return(NA)
    }

    if (!all(vert_min_lat == -Inf, vert_max_lat == Inf))
        hysplit_df <- hysplit_df[hysplit_df[[hs_clat]] > vert_min_lat &
                                 hysplit_df[[hs_clat]] < vert_max_lat,]
    if (nrow(hysplit_df) == 0) {
        warning("No trajectory vertex meets the longitude filter!")
        return(NA)
    }

    # Build a grid. NOTE: Assume the first vertex is the grid's origin.
    origin_x <- hysplit_df[1, hs_clon]
    origin_y <- hysplit_df[1, hs_clat]
    if (!all(min_lon < origin_x, 
             min_lat < origin_y,
             origin_x < max_lon, 
             origin_y < max_lat))
        stop(paste("Invalid grid for trajectories:", files, sep = "\n"))

    aoi_grid <- build_grid(
        origin_lon = origin_x,
        origin_lat = origin_y,
        min_lon = min_lon,
        max_lon = max_lon,
        min_lat = min_lat,
        max_lat = max_lat,
        grid_resolution = grid_resolution,
        crs = crs
    )

  # Build a sf object (point) using the trajectories' vertices.
    hysplit_sf <- sf::st_as_sf(
        x = hysplit_df, 
        coords = c(hs_clon, hs_clat), 
        crs = crs
    )

    # Cross the grid and trajetories' vertices.
    # NOTE: s2 is slow at running st_intersection.
    s2 <- sf::sf_use_s2()
    suppressMessages({ sf::sf_use_s2(FALSE) })
    sf::st_agr(hysplit_sf) <- sf::st_agr(aoi_grid) <- "constant"
    hysplit_sf <- sf::st_intersection(x = hysplit_sf, y = aoi_grid)
    suppressMessages({ sf::sf_use_s2(s2) })

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

