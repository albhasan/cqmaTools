# compute the influence area
library(sf)
library(terra)




#---- Util ----

#' Build a vector from the origin to the min and max values.
#' @param o A numeric. The origin.
#' @param min A numeric. The mininum value.
#' @param max A numeric. The maximum value.
.grid_helper <- function(o, min, max, res) {
  sort(c(seq(from = o, to = max, by = res), 
         seq(from = o, to = min, by = -res)[-1]))
}

# Build a grid with origin at the position given by hysplit's first row.
build_grid <- function(origin_x, origin_y, min_lon, max_lon, min_lat, 
                       max_lat, grid_resolution) {

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


build_density_grid <- function(files,
                               hs_skip = 7,
                               hs_cnames = HYSPLIT.COLNAMES,
                               crs = 4326,
                               min_lon = -80, max_lon = -30,
                               grid_resolution = 2,
                               min_lat = -40, max_lat = 10
                               ) {


  # Read hyspit files into a dataframe.
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

  # Build a grid.
  aoi_grid <- build_grid(origin_x = hysplit_df[1, "lon"],
                         origin_y = hysplit_df[1, "lat"],
                         min_lon, max_lon, min_lat, 
                         max_lat, grid_resolution)

  # Build a point sf object from hysplit's trajectories.
  hysplit_sf <-
    sf::st_as_sf(
      x = hysplit_df,
      coords = c("lon", "lat"),
      crs = crs
    )

  # Compute trajectory density. Cross grid and trajetories' vertices.
  # NOTE: s2 is very slow at running st_intersection.
  s2 <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  hysplit_sf <- sf::st_intersection(x = hysplit_sf, y = aoi_grid)
  sf::sf_use_s2(s2)
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


#' Process the given files.
#' @param siteyear A character of lenght 2. The site abbreviation and a year.
#' @param files    A character. Paths to hysplit files.
#' @param min_lon  A numeric. Grid's mininum longitude value.
#' @param max_lon  A numeric. Grid's maximum longitude value.
#' @param min_lat  A numeric. Grid's mininum latitude value.
#' @param max_lat  A numeric. Grid's maximum latitude value.
#' @param grid_resolution A numeric. Grid's resolution (in degrees).
#' @param crs A numeric. An EPSG code of a geographic reference
#' system. This is used for creating the grid and for reading the given files.
#' @param hs_skip     A numeric. Number of rows to skip from each hysplit file.
#' @param hs_cnames A character. The names of the columns in the hysplit files.
compInfArea <- function(files,
                        min_lon = -80, max_lon = -30,
                        min_lat = -40, max_lat = 10,
                        grid_resolution = 2,
                        crs = 4326,
                        hs_skip = 7,
                        hs_cnames = HYSPLIT.COLNAMES,
                        plot_title = "Area of Influence") {

  # Build a density grid.
  aoi_grid_r <- build_density_grid(
    files = files,
    hs_skip = hs_skip,
    hs_cnames = hs_cnames,
    crs = crs,
    min_lon = min_lon,
    max_lon = max_lon,
    grid_resolution = grid_resolution,
    min_lat = min_lat,
    max_lat = max_lat
  )

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

  nbreaks <- 10
  #d <- r1
  intBreaks <- seq(2, 8, length.out = nbreaks) 
  plot(aoi_grid_r, 
       col = rev(heat.colors(nbreaks)),
       breaks = intBreaks, 
       main = plot_title) 
  lines(aoi_xy[chrows,], col = "red")
  maps::map("world", xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat), add = TRUE)

  return(chrows[chrows,])
}


#---- Script ----

#hysplit_path <- "/home/alber/Documents/inpe/lagee/all_co2"
hysplit_path <- "/home/alber/Documents/inpe/lagee/lucas"
stopifnot("Hysplit directory not found" = dir.exists(hysplit_path))


# build a matrix of sites and years
siteyear_mt <- do.call(rbind, unique(lapply(list.files(hysplit_path),
                                            function(x){
  spl <- strsplit(x, split = "_")[[1]]
  return(c("site" = spl[1], "year" = spl[2]))
})))

hotspots <- list()

for(i in seq(nrow(siteyear_mt))) {

  # Select a site and a year.
  siteyear <- siteyear_mt[i, ]

  # Filter files by site and year.
  files <- list.files(
    path = hysplit_path,
    #pattern = paste(siteyear, collapse = "_"),
    full.names = TRUE,
    recursive = TRUE
  )

  # Process the files.
  hotspots[[paste(siteyear, collapse = "-")]] <-
    compInfArea(
      files = files
    )

}




# Test Republica Dominicana. 

compInfArea(
  files = files,
  min_lon = -74,
  max_lon = -68,
  min_lat = 16,
  max_lat = 22,
  grid_resolution = 0.05,
  crs = 4326,
  hs_skip = 7,
  hs_cnames = HYSPLIT.COLNAMES
)

