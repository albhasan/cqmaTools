#' Build a convex hull around a raster
#'
#' @description 
#' Build a convex hull round the pixels in the given raster which are not NULL.
#' 
#' @param r a raster (terra).
#'
#' @return a polygon (sf).
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



#' Smooth raster using NA neighbors
#' 
#' @description
#' Transform the cells into NA depending on the number of NA neighbors.
#'
#' @param r a raster (terra).
#' @param w an integer. The window size.
#' @param threshold an integer. 
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

