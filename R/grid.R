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



#' Rasterize a grid
#'
#' @description
#' Transform a grid (sf object) into a raster (terra object).
#'
#' @param grid_sf an sf object of type polygon.
#' @param grid_resolution a numeric of length 1 or 2.
#' @param cname a character(1). Name of an attribute in grid_sf.
#'
#' @return a raster (terra object).
#'
#' @seealso [terra::rasterize] which this function wraps.
#'
#' @export
#'
grid_to_raster <- function(grid_sf, grid_resolution, cname) {

    stopifnot("Expected character(1) for `cname`" = length(cname) == 1)
    stopifnot("`cname` not found in grid_sf!" = cname %in% colnames(grid_sf))
    stopifnot("Expected an sf object for a grid!" = 
        inherits(grid_sf, what = "sf"))
    stopifnot("Expected a grid of type POLYGON" = 
        as.character(sf::st_geometry_type(grid_sf, by_geometry = FALSE)) %in% 
            "POLYGON")

    grid_vect <- terra::vect(grid_sf)
    template <- terra::rast(grid_vect, resolution = grid_resolution)
    grid_r <- terra::rasterize(grid_vect, y = template, field = cname)

    return(grid_r)

}

