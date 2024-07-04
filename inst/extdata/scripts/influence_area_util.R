#' @title Plot the area of influence
#' @name plot_aoi
#' @author Alber Sánchez, \email{alber.ipia@@inpe.br}
#'
#' @description Plot the given raster with a background of countries.
#' 
#' @param aoi_r A raster (terra).
#' @param nbreaks An integer. The number of breaks to use in the color scale.
#' @param breaks An integer. Value intervals for the breaks.
#' @param add_cv Should we add the convex hull of the area of influence?
#' @param min_lon  A numeric. Grid's mininum longitude value.
#' @param max_lon  A numeric. Grid's maximum longitude value.
#' @param min_lat  A numeric. Grid's mininum latitude value.
#' @param max_lat  A numeric. Grid's maximum latitude value.
#' @param plot_title Title for the plot.
#'
#' @return The given raster.
#' 
#' @export
#'
plot_aoi <- function(aoi_r, nbreaks = 5, breaks = NA, add_cv = FALSE, 
                     min_lon, max_lon, min_lat, max_lat, 
                     plot_title = "Influence area") {
  if (is.na(breaks)) {
    v_range <- range(aoi_r[], na.rm = TRUE)
    breaks <- seq(from = v_range[1], to = v_range[2], length.out = nbreaks + 1)
  }
  plot(aoi_r, col = rev(heat.colors(nbreaks)),
       breaks = breaks, main = plot_title)
  if (add_cv)
      lines(cv_sf, col = "red")
  maps::map("world", xlim = c(min_lon, max_lon),
            ylim = c(min_lat, max_lat), add = TRUE)
  invisible(aoi_r)
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
                                   plot_area = TRUE, 
                                   plot_title = "Area of Influence") {

  # Compute the number of trajectory vertices in each cell in the grid. 
  aoi_grid_r <-
    compute_frequency_grid(files = files, hs_skip = hs_skip, 
                           hs_cnames = hs_cnames, crs = crs, min_lon = min_lon, 
                           max_lon = max_lon, min_lat = min_lat, 
                           max_lat = max_lat,
                           grid_resolution = grid_resolution)

  # Compute the logarithm.
  aoi_grid_r <- log(aoi_grid_r)

  # Filter
  aoi_grid_r <- terra::mask(aoi_grid_r, mask = aoi_grid_r >= 5.5)

  # Build a convex hull.
  cv_sf <- raster2convexhull(aoi_grid_r)

  # Plot.
  if (plot_area)
    plot_aoi(aoi_grid_r, nbreaks = 5, breaks = NA, add_cv = FALSE, 
             min_lon = min_lon, max_lon = max_lon, 
             min_lat = min_lat, max_lat = max_lat,
             plot_title = "Influence area")

  invisible(cv_sf)
}
