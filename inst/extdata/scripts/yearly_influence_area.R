# compute the yearly influence area

#---- TODO ----

# remove
library(devtools)
devtools::load_all()

#---- Load libraries ----
library(dplyr)
library(raster)

#---- Configuration ----

trajectory_dir <- "/home/lagee/Documents/alber/test/hysplitsimulations/all_co2"
station_data_dir <- "/home/lagee/Dropbox/DADOS LaGEE/ScriptsR_Figures/influence_area_1300/by_year/stn_byia/man"
out_dir <- "/home/lagee/Dropbox/DADOS LaGEE/ScriptsR_Figures/influence_area_1300/by_year_mean/man"
HYSPLIT.COLNAMES <- c("V1", "V2", "year", "month", "day", "hour", "min",
                      "V8", "V9", "lat", "lon", "height", "pressure")
grid_resolution <- 1 # DO NOT CHANGE!
grid_lon_range <- c(-80, -30)
grid_lat_range <- c(-30, 10)

site_location_tb <- tribble(
  ~site, ~longitude, ~latitude, ~site_height,
 #"ALF",     -55.78,    -8.822,    208,
  "ALF",     -56.78,    -8.822,    208, # Luciana's new table
  "RBA",     -64.74,    -9.02,     120,
  "SAN",     -54.95,    -2.85,     152,
 #"TAB",     -68.9,     -5.74,     204,
  "TAB",     -69.9,     -5.74,     204, # Luciana's new table
 #"MAN",     -60.21,    -2.6,       85,
  "MAN",     -59.82,    -2.5,       85, # Luciana's new table
  "TEF",     -66.5,     -3.68,      90
)


# Number of neighbor cells to take into account express as a "radius". It's a
# "radius" where i.e. 1 means a 3x3, 2 means 5x5, etc
neighbor_radius <- 1

# Maximum number of missing neighbors (NAs) of each cell.
max_missing_neighbors <- 4

#---- compute the statistics ----

#' Helper function for getting the height range of a trajectory.
#'
#' @param data_tb A tibble. Trajectory data.
#' @return        A tibble.
get_height_range <- function(data_tb) {
    h_range <- data_tb %>%
        dplyr::pull(height) %>%
        range()
    tibble::tibble(height_min = h_range[[1]],
                   height_max = h_range[[2]])
}

# Create a table of files.
trajectory_tb <- trajectory_dir %>%
  list.files(full.names = TRUE) %>%
  tibble::enframe(name = NULL) %>%
  dplyr::rename(file_path = value) %>%
  dplyr::mutate(filename = basename(file_path)) %>%
  tidyr::separate(filename, c("site", "traj_year", "traj_month", "traj_day",
                              "traj_hour", "traj_height"),
                  sep = '_') %>%
  dplyr::mutate(data = purrr::map(file_path,  readr::read_delim, delim = ' ',
                                  skip = 7,
                                  trim_ws = TRUE,
                                  col_names = HYSPLIT.COLNAMES,
                                  col_types = readr::cols(V1 = readr::col_character(),
                                                          V2 = readr::col_character(),
                                                          year = readr::col_integer(),
                                                          month = readr::col_integer(),
                                                          day = readr::col_integer(),
                                                          hour = readr::col_integer(),
                                                          min = readr::col_integer(),
                                                          V8 = readr::col_character(),
                                                          V9 = readr::col_character(),
                                                          lat = readr::col_double(),
                                                          lon = readr::col_double(),
                                                          height = readr::col_double(),
                                                          pressure = readr::col_double()))) %>%
  # Filter trajectories between 50mts and 1500mts respect to each site.
  dplyr::mutate(height_range = purrr::map(data, get_height_range)) %>%
  tidyr::unnest(height_range) %>%
  dplyr::left_join(site_location_tb, by = "site") %>%
  dplyr::mutate(site_height_min = site_height + 50,
                site_height_max = site_height + 1500) %>%
  dplyr::filter(height_min >= site_height_min,
                height_max <= site_height_max) %>%
  dplyr::select(-height_min, -height_max,
                -site_height_min, -site_height_max, -site_height) %>%
  tidyr::unnest(cols = c(data)) %>%
  dplyr::filter(lon >= grid_lon_range[1], lon <= grid_lon_range[2],
                lat >= grid_lat_range[1], lat <= grid_lat_range[2]) %>%
  dplyr::mutate(sp_index = stringr::str_c(floor(lon), '_', floor(lat)),
                traj_trimester = paste(traj_year,findInterval(traj_month, seq(1, 12, by = 3)), sep = '_'),
                traj_trimester_total = paste(findInterval(traj_month, seq(1, 12, by = 3)), sep = '_')) %>%
  #dplyr::filter(height <= 1300) %>%
  #ensurer::ensure_that(all(.$traj_trimester > 0 && .$traj_trimester < 5))
  ensurer::ensure_that(all(.$traj_trimester_total > 0 && .$traj_trimester_total < 5))

traj_by_year <- trajectory_tb %>%
  dplyr::group_by(site, traj_year, sp_index) %>%
  dplyr::summarize(n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename("total_year" = `n()`)

traj_by_trimester <- trajectory_tb %>%
  dplyr::group_by(site, traj_trimester, sp_index) %>%
  dplyr::summarize(n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename("total_trimester" = `n()`)

traj_total <- trajectory_tb %>%
  dplyr::group_by(site, sp_index) %>%
  dplyr::summarize(n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename("total_site" = `n()`)

site_years <- traj_by_year %>%
  dplyr::select(site, traj_year) %>%
  dplyr::distinct() %>%
  dplyr::group_by(site) %>%
  dplyr::summarize(n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename("years_per_site" = `n()`)

site_trimester <- traj_by_trimester %>%
  dplyr::select(site, traj_trimester) %>%
  dplyr::distinct() %>%
  dplyr::group_by(site) %>%
  dplyr::summarize(n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename("trimester_per_site" = `n()`)

traj_by_year <- traj_by_year %>%
  dplyr::left_join(traj_total, by = c("site", "sp_index")) %>%
  dplyr::left_join(site_years, by = "site")

traj_by_trimester <- traj_by_trimester %>%
  dplyr::left_join(traj_total, by = c("site", "sp_index")) %>%
  dplyr::left_join(site_trimester, by = "site")

my_sites <-  traj_by_trimester %>%
  dplyr::mutate(year = stringr::str_sub(traj_trimester, 1, 4)) %>%
  dplyr::select(site, year, trimester = traj_trimester) %>%
  dplyr::distinct() %>%
  dplyr::arrange(site, year, trimester)

traj_by_trimester_total <- trajectory_tb %>%
  dplyr::group_by(site, traj_trimester_total, sp_index) %>%
  dplyr::summarize(n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename("trimester" = `n()`)

traj_by_mean_trimester <- traj_by_trimester_total %>%
  dplyr::left_join(site_years, by = "site")

#---- build a raster grid ----

new_raster <- function(grid_resolution, grid_lon_range, grid_lat_range) {
  ext <- raster::extent(min(grid_lon_range), max(grid_lon_range),
                        min(grid_lat_range), max(grid_lat_range))
  grid_raster <- raster(ext)
  res(grid_raster) <- c(grid_resolution, grid_resolution)
  projection(grid_raster) <- CRS("+init=epsg:4326")
  grid_raster[] <- 0
  return(grid_raster)
}

grid_table <- expand.grid(seq(min(grid_lon_range), by = grid_resolution, length.out = diff(grid_lon_range)),
                          seq(min(grid_lat_range), by = grid_resolution, length.out = diff(grid_lat_range))) %>%
  #grid_table <- expand.grid(seq(-180, by = grid_resolution, length.out = 361),
  #                        seq(-90, by = grid_resolution, length.out = 181)) %>%
  tibble::as_tibble() %>%
  dplyr::rename(lon = Var1, lat = Var2) %>%
  dplyr::mutate(sp_index = stringr::str_c(floor(lon), '_', floor(lat))) %>%
  dplyr::arrange(dplyr::desc(lat), lon)

#---- link the data to the raster ----

my_treshold <- 0.025

#----- Plot total ----
for (my_site in unique(dplyr::pull(my_sites, site))) {
    print(sprintf("Site: %s Treshold: %s", my_site, my_treshold))
    data_vector <- traj_by_year %>%
      dplyr::filter(site == my_site) %>%
      dplyr::filter(traj_year == dplyr::first(dplyr::pull(., traj_year))) %>%
      dplyr::mutate("mean_trajectory" = total_site / years_per_site,
                    "treshold" = max(mean_trajectory) * my_treshold) %>%
      dplyr::filter(mean_trajectory >= treshold) %>%
      ensurer::ensure_that(nrow(.) > 0, err_desc = "No data found!") %>%
      ensurer::ensure_that(length(unique(.$site)) == 1,
                           length(unique(.$traj_year)) == 1,
                           err_desc = "Invalid trajectory site or year") %>%
      right_join(grid_table, by = "sp_index")

    grid_raster <- new_raster(grid_resolution, grid_lon_range, grid_lat_range)
    grid_raster[] <- data_vector %>%
      dplyr::pull(mean_trajectory)

    #---- Count neighbors ----

    na_neighbors <- raster::focal(grid_raster,
                                  w = matrix(rep(1, (neighbor_radius * 2 + 1)^2),
                                             ncol = (neighbor_radius * 2 + 1)),
                                  fun = function(x){sum(is.na(x))})
    data_vector <- data_vector %>%
      dplyr::mutate(missing_neigbors = na_neighbors[],
                    mean_trajectory = ifelse(missing_neigbors <= max_missing_neighbors,
                                             mean_trajectory, NA))

    grid_raster <- new_raster(grid_resolution, grid_lon_range, grid_lat_range)
    grid_raster[] <- data_vector %>%
      dplyr::pull(mean_trajectory)

    #---- Save the plot ----
    out_file <- paste0(my_site, '_total')
    png(filename = file.path(out_dir, paste0(out_file, ".png")))
    plot(grid_raster, main =  sprintf("Site: %s Treshold: %s",
                                      my_site, my_treshold))
    points(dplyr::select(site_location_tb, longitude, latitude))
    maps::map("world",
              xlim = grid_lon_range,
              ylim = grid_lat_range,
              add = TRUE)
    dev.off()
    raster::writeRaster(grid_raster,
                        filename = file.path(out_dir,  paste0(out_file, ".tif")),
                        options = c('TFW=YES'))

    data_vector %>%
      write.table(file = file.path(out_dir, paste0(out_file, ".txt")))
    rm(data_vector)

}


#----- Plot by year ----


for (my_site in unique(dplyr::pull(my_sites, site))) {
  for (my_year in unique(dplyr::pull(dplyr::filter(my_sites, site == my_site),
                                     year))) {
    print(sprintf("Site: %s Year: %s Treshold: %s", my_site, my_year, my_treshold))
    data_vector <- traj_by_year %>%
      dplyr::filter(site == my_site, traj_year == my_year) %>%
      dplyr::mutate("mean_trajectory" = total_year/ 1,
                    "treshold" = max(mean_trajectory) * my_treshold) %>%
      dplyr::filter(mean_trajectory >= treshold) %>%
      ensurer::ensure_that(nrow(.) > 0, err_desc = "No data found!") %>%
      ensurer::ensure_that(length(unique(.$site)) == 1,
                           length(unique(.$traj_year)) == 1,
                           err_desc = "Invalid trajectory site or year") %>%
      right_join(grid_table, by = "sp_index")

    grid_raster <- new_raster(grid_resolution, grid_lon_range, grid_lat_range)
    grid_raster[] <- data_vector %>%
      dplyr::pull(mean_trajectory)

    #---- Count neighbors ----
    na_neighbors <- raster::focal(grid_raster,
                                  w = matrix(rep(1, (neighbor_radius * 2 + 1)^2),
                                             ncol = (neighbor_radius * 2 + 1)),
                                  fun = function(x){sum(is.na(x))})
    data_vector <- data_vector %>%
      dplyr::mutate(missing_neigbors = na_neighbors[],
                    mean_trajectory = ifelse(missing_neigbors <= max_missing_neighbors,
                                             mean_trajectory, NA))

    grid_raster <- new_raster(grid_resolution, grid_lon_range, grid_lat_range)
    grid_raster[] <- data_vector %>%
      dplyr::pull(mean_trajectory)

    #---- Save the plot ----
    out_file <- paste0(my_site, '_', my_year)
    png(filename = file.path(out_dir, paste0(out_file, ".png")))
    plot(grid_raster, main =  sprintf("Site: %s Year: %s Treshold: %s",
                                      my_site, my_year, my_treshold))
    points(dplyr::select(site_location_tb, longitude, latitude))
    maps::map("world",
              xlim = grid_lon_range,
              ylim = grid_lat_range,
              add = TRUE)
    dev.off()
    raster::writeRaster(grid_raster,
                        filename = file.path(out_dir,  paste0(out_file, ".tif")),
                        options = c('TFW=YES'))

    data_vector %>%
      write.table(file = file.path(out_dir, paste0(out_file, ".txt")))
    rm(data_vector)
  }
}


#----- Plot by trimester ----


for (my_site in unique(dplyr::pull(my_sites, site))) {
  for (my_trimester in unique(dplyr::pull(dplyr::filter(my_sites,
                                                        site == my_site),
                                     trimester))) {
    print(sprintf("Site: %s Trimester: %s Treshold: %s", my_site, my_trimester,
                  my_treshold))
    data_vector <- traj_by_trimester %>%
      dplyr::filter(site == my_site, traj_trimester == my_trimester) %>%
      dplyr::mutate("mean_trajectory" = total_trimester / 1,
                    "treshold" = max(mean_trajectory) * my_treshold) %>%
      dplyr::filter(mean_trajectory >= treshold) %>%
      ensurer::ensure_that(nrow(.) > 0, err_desc = "No data found!") %>%
      ensurer::ensure_that(length(unique(.$site)) == 1,
                           length(unique(.$traj_trimester)) == 1,
                           err_desc = "Invalid trajectory site or trimester") %>%
      right_join(grid_table, by = "sp_index")

    grid_raster <- new_raster(grid_resolution, grid_lon_range, grid_lat_range)
    grid_raster[] <- data_vector %>%
      dplyr::pull(mean_trajectory)

    #---- Count neighbors ----
    na_neighbors <- raster::focal(grid_raster,
                                  w = matrix(rep(1, (neighbor_radius * 2 + 1)^2),
                                             ncol = (neighbor_radius * 2 + 1)),
                                  fun = function(x){sum(is.na(x))})
    data_vector <- data_vector %>%
      dplyr::mutate(missing_neigbors = na_neighbors[],
                    mean_trajectory = ifelse(missing_neigbors <= max_missing_neighbors,
                                             mean_trajectory, NA))

    grid_raster <- new_raster(grid_resolution, grid_lon_range, grid_lat_range)
    grid_raster[] <- data_vector %>%
      dplyr::pull(mean_trajectory)

    #---- Save the plot ----
    out_file <- paste0(my_site, '_', my_trimester)
    png(filename = file.path(out_dir, paste0(out_file, ".png")))
    plot(grid_raster, main =  sprintf("Site: %s Trimester: %s Treshold: %s",
                                      my_site, my_trimester, my_treshold))
    points(dplyr::select(site_location_tb, longitude, latitude))
    maps::map("world",
              xlim = grid_lon_range,
              ylim = grid_lat_range,
              add = TRUE)
    dev.off()
    raster::writeRaster(grid_raster,
                        filename = file.path(out_dir,  paste0(out_file, ".tif")),
                        options = c('TFW=YES'))

    data_vector %>%
      write.table(file = file.path(out_dir, paste0(out_file, ".txt")))
    rm(data_vector)
  }
}

#----- Plot by total trimester ----


for (my_site in unique(dplyr::pull(my_sites, site))) {
  for (my_trimester in unique(dplyr::pull(dplyr::filter(traj_by_mean_trimester,
                                                        site == my_site),
                                          traj_trimester_total))) {
    print(sprintf("Site: %s Trimester: %s Treshold: %s", my_site, my_trimester,
                  my_treshold))
    data_vector <- traj_by_mean_trimester %>%
      dplyr::filter(site == my_site, traj_trimester_total == my_trimester) %>%
      dplyr::mutate("mean_trajectory" = trimester / years_per_site,
                    "treshold" = max(mean_trajectory) * my_treshold) %>%
      dplyr::filter(mean_trajectory >= treshold) %>%
      ensurer::ensure_that(nrow(.) > 0, err_desc = "No data found!") %>%
      ensurer::ensure_that(length(unique(.$site)) == 1,
                           length(unique(.$traj_trimester_total)) == 1,
                           err_desc = "Invalid trajectory site or trimester") %>%
      right_join(grid_table, by = "sp_index")

    grid_raster <- new_raster(grid_resolution, grid_lon_range, grid_lat_range)
    grid_raster[] <- data_vector %>%
      dplyr::pull(mean_trajectory)

    #---- Count neighbors ----
    na_neighbors <- raster::focal(grid_raster,
                                  w = matrix(rep(1, (neighbor_radius * 2 + 1)^2),
                                             ncol = (neighbor_radius * 2 + 1)),
                                  fun = function(x){sum(is.na(x))})
    data_vector <- data_vector %>%
      dplyr::mutate(missing_neigbors = na_neighbors[],
                    mean_trajectory = ifelse(missing_neigbors <= max_missing_neighbors,
                                             mean_trajectory, NA))

    grid_raster <- new_raster(grid_resolution, grid_lon_range, grid_lat_range)
    grid_raster[] <- data_vector %>%
      dplyr::pull(mean_trajectory)

    #---- Save the plot ----
    out_file <- paste0(my_site, '_', my_trimester)
    png(filename = file.path(out_dir, paste0(out_file, ".png")))
    plot(grid_raster, main =  sprintf("Site: %s Trimester: %s Treshold: %s",
                                      my_site, my_trimester, my_treshold))
    points(dplyr::select(site_location_tb, longitude, latitude))
    maps::map("world",
              xlim = grid_lon_range,
              ylim = grid_lat_range,
              add = TRUE)
    dev.off()
    raster::writeRaster(grid_raster,
                        filename = file.path(out_dir,  paste0(out_file, ".tif")),
                        options = c('TFW=YES'))

    data_vector %>%
      write.table(file = file.path(out_dir, paste0(out_file, ".txt")))
    rm(data_vector)
  }
}

