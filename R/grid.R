#' @title Build a grid
#'
#' @description 
#' Build a grid that includes the given origin as a vertex.
#'
#' @param origin_lon,origin_lat a numeric(1). Coordinate of origin.
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
build_grid <- function(origin_lon, origin_lat, min_lon, max_lon, min_lat, 
                       max_lat, grid_resolution, crs) {

    stopifnot("The origin must fall in the given ranges!" = all(
        min_lon <= origin_lon, origin_lon <= max_lon,
        min_lat <= origin_lat, origin_lat <= max_lat
    ))

    lon_grid <- grid_helper(o = origin_lon, min = min_lon, max = max_lon,
        res = grid_resolution)
    lat_grid <- grid_helper(o = origin_lat, min = min_lat, max = max_lat,
        res = grid_resolution)

    aoi_grid <- sf::st_as_sf(sf::st_make_grid(
        x = sf::st_bbox(c(xmin = min(lon_grid), xmax = max(lon_grid),
            ymin = min(lat_grid), ymax = max(lat_grid)),
            crs = sf::st_crs(crs)),
        cellsize = grid_resolution
    ))
    aoi_grid["gid"] <- seq(nrow(aoi_grid))

    return(aoi_grid)

}


#' Build a vector that passes through the origin
#'
#' @description
#' Build a vector from the minimum to the maximum value ensuring that the
#' given origin is a value in the returned vector.
#'
#' @param o   a numeric(1). Origin.
#' @param min a numeric(1). Mininum value.
#' @param max a numeric(1). Maximum value.
#' @param res a numeric(1). Grid resultuion.
#'
#' @return    A numeric.
#'
grid_helper <- function(o, min, max, res) {
  sort(c(seq(from = o, to = max, by = res), 
         seq(from = o, to = min, by = -res)[-1]))
}

