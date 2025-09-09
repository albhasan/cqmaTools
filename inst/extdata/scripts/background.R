#!/usr/bin/env Rscript

###############################################################################
# BACKGROUND
# This script streamlines the data flow of the LAGEE LAB at INPE
#------------------------------------------------------------------------------
# CMQA DATA FLOW
# NOTES:
# - A vertical profile is made of jars (12 or 17). Each jar is a sample taken
#   at certain height.
# - A vertical profile corresponds to one flight.
# - In the lab, each jar is analyzed and gas concentration is measured.
# - Each jar corresponds to a height in a profile.
# - A hysplit trajectory is computed for each jar.
# - Each trajectory reaches the sea at some point.
# - We're interested ONLY in the first point of each trajectory' that is over
#   the sea.
# - We use this point to interpolate a gas concentration.
#-------------------------------------------------------------------
# TODO:
# - Pay attention to the profile 2022-07-18.
# - Run more than one GHG at the same time.
# - Add gas name to the figures' title.



suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(rlog))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tools))

library(cqmaTools)



rlog::log_info("-------------------------------------------------------------")
rlog::log_info("background.R")



rlog::log_info("Reading configuration...")



station_location_file <- system.file("extdata", "station_location.csv",
                                     package = "cqmaTools")
station_dir <- "/home/alber/Documents/data/r_packages/cqmaTools/stations/data"
hysplit_dir <- "/home/alber/Documents/data/r_packages/cqmaTools/trajectories"
out_dir <- "/home/alber/Downloads/tmp"


# Filter trajectories by coordinates.
traj_min_height <- -Inf
traj_max_height <- Inf
traj_min_lon    <- -Inf
traj_max_lon    <- Inf
traj_min_lat    <- -Inf
traj_max_lat    <- Inf

# Filter by trajectories' vertices.
vert_min_height <- -Inf
vert_max_height <- Inf
vert_min_lon    <- -Inf
vert_max_lon    <- Inf
vert_min_lat    <- -Inf
vert_max_lat    <- Inf
min_per_vert_in_hrange <- 0

# Trajector-file's rows to take into account during calculations.
from_row <- 1
to_row   <- 240


# Column names used in the trajectories.
traj_clon <- "longitude"
traj_clat <- "latitude"
traj_cheight <- "height"

# Vector data used to intersect the trajectories.
limit_file <-
  "/home/alber/Documents/github/cqmaTools/inst/extdata/limit/fake_limit.shp"

# Number of observations to take into account while interpolating missing
# values.
n_obs_interpolation <- 2L

# Map's longitude and latitude ranges.
map_lon_range <- c(-75, -30)
map_lat_range <- c(-35, 10)

# Maximum number of trajectories in a map's legend.
map_max_traj_leg <- 15

# Image size for plots (landscape by default).
plot_width  <- 297
plot_height <- 210
plot_units  <- "mm"

# Maximum number of legend items (back trajectories) in plot maps
n_max_bkraj_map_legend <- 20

# Column names used in the stations.
station_clon <- "lon"
station_clat <- "lat"



rlog::log_info("Loading data...")



stations_lonlat_tb <-
  readr::read_csv(station_location_file, col_types = "cdd")

limit_sf <- sf::read_sf(dsn = limit_file)



rlog::log_info("Running validations...")



stopifnot("Directory with stations' data not found!" = dir.exists(station_dir))
stopifnot("Station location file not found!" =
            file.exists(station_location_file))
stopifnot("Hysplit directory not found!" = dir.exists(hysplit_dir))
stopifnot("Limit vector not found!" = file.exists(limit_file))
stopifnot("Line expected!" = "LINESTRING" %in% sf::st_geometry_type(limit_sf))
stopifnot("Output directory with not found!" = dir.exists(out_dir))



rlog::log_info("Loading utilitary functions...")



rlog::log_info("Reading metereological station data...")



stations_tb <-
  station_dir %>%
  list.files(
    full.names = TRUE,
    recursive = FALSE,
    include.dirs = FALSE
  ) %>%
  dplyr::as_tibble() %>%
  dplyr::rename(file_path = "value") %>%
  dplyr::mutate(
    file_name = basename(file_path),
    file_name = tools::file_path_sans_ext(file_name)
  ) %>%
  tidyr::separate(
    col = file_name,
    into = c("root", "gas"),
    sep = "[.]"
  ) %>%
  dplyr::mutate(
    name = stringr::str_sub(root, start = 1, end = 3),
    name = stringr::str_to_upper(name)
  ) %>%
  dplyr::select(-root) %>%
  dplyr::full_join(y = stations_lonlat_tb, by = "name") %>%
  dplyr::arrange(station_clat) %>%
  dplyr::mutate(data_df = purrr::map(file_path, read_station_file)) %>%
  tidyr::unnest(data_df) %>%
  dplyr::mutate(
    date = lubridate::date_decimal(date),
    date = lubridate::as_date(date)
  )

stopifnot("Missing station data!" = any(!is.na(stations_tb[["name"]])))



rlog::log_info("Reading backtrajectory data...")



backtrajectories_tb <-
  hysplit_dir %>%
  # Get trajectory files.
  list.files(
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE,
    pattern = TRAJECTORY.FILENAME.PATTERN
  ) %>%
  dplyr::as_tibble() %>%
  dplyr::rename(file_path = "value") %>%
  dplyr::mutate(file_name = basename(file_path)) %>%
  # Parse file name attributes.
  tidyr::separate(
    col = file_name,
    into = c("site", "year", "month", "day", "hour", "end_height"),
    sep = "[_]"
  ) %>%
  tidyr::unite(year, month, day, col = "sample_date", sep = "-") %>%
  dplyr::mutate(
    hour = paste0(hour, ":00:00"),
    end_height = as.double(end_height)
  ) %>%
  tidyr::unite(sample_date, hour, col = "sample_date", sep = " ") %>%
  dplyr::mutate(sample_date = lubridate::as_datetime(sample_date)) %>%
  # Read trajectory data.
  dplyr::mutate(data_df = purrr::map(
    .x = file_path,
    .f = read_trajectory_file,
    cnames = names(TRAJECTORY.COLNAMES),
    ctypes = TRAJECTORY.COLNAMES,
    skip = TRAJECTORY.SKIP
  )) %>%
  # Check for trajectories off limits.
  dplyr::mutate(
    data_df = filter_traj(
      traj = data_df,
      clon = traj_clon,
      clat = traj_clat,
      cheight = traj_cheight,
      traj_min_lon = traj_min_lon,
      traj_max_lon = traj_max_lon,
      traj_min_lat = traj_min_lat,
      traj_max_lat = traj_max_lat,
      traj_min_height = traj_min_height,
      traj_max_height = traj_max_height
    )
  ) %>%
  # Filter trajectories' vertices.
  # Cut trajectories to a maximum number of hours back.
  dplyr::mutate(
    data_df = filter_traj_vertices(
      traj = data_df,
      clon = traj_clon,
      clat = traj_clat,
      cheight = traj_cheight,
      from_row = from_row,
      to_row = to_row
    )
  ) %>%
  # Find the intersection row for each trajectory.
  dplyr::mutate(
    cross_row = as.integer(
      intersect_trajectories(
        traj_ls = data_df,
        limit = limit_sf,
        clon = traj_clon,
        clat = traj_clat,
        crs = 4326,
        row_after = TRUE
      )
    ),
  )



rlog::log_info("Keeping backtrajectories without GHG concentration...")



bt_missing_tb <-
  backtrajectories_tb %>%
  dplyr::filter(is.na(cross_row))



rlog::log_info("Listing observed GHG gasses...")



station_gasses <-
  stations_tb %>%
  dplyr::pull(gas) %>%
  unique() %>%
  sort()

stopifnot("Only one gas is currently supported!" = length(station_gasses) == 1)



rlog::log_info("Estimating backtraj's GHG concentration at met stations...")



backtrajectories_tb <-
  backtrajectories_tb %>%
  # Remove trajectories that do not reach the line where metereological data is
  # interpolated.
  dplyr::filter(cross_row > 0) %>%
  # Clip trajectories using cross_row.
  dplyr::mutate(
    data_df = purrr::map2(
      .x = data_df,
      .y = cross_row,
      .f = function(data_df, n) {
        return(dplyr::slice_head(data_df, n = n))
      }
    )
  ) %>%
  # Get data from the crossing between the backtrajectory and the limit.
  dplyr::mutate(
    cross_lat_date = purrr::map2(
      .x = data_df,
      .y = cross_row,
      .f = function(data_df, cross_row) {
        row_df <- data_df[cross_row, c(traj_clat, "date")]
        colnames(row_df) <- paste("cross", colnames(row_df), sep = "_")
        return(row_df)
      }
    )
  ) %>%
  tidyr::unnest(cols = tidyselect::all_of("cross_lat_date")) %>%
  # Find the stations matching each backtrajectry and interpolete gas values.
  # TODO: Estimate the time from trajectories crossing the limit to the
  # station's measurement. Filter trajectories if the time difference is too
  # large.
  dplyr::mutate(
    ghg_stations = purrr::map2(
      .x = cross_latitude,
      .y = cross_date,
      .f = estimate_ghg,
      stations_tb = stations_tb,
      clat = station_clat,
      cgas = "gas",
      cname = "name",
      cvalue = "value",
      new_col = "ghg_stations"
    ),
    ghg_stations = vector_to_tibble(ghg_stations)
  ) %>%
  tidyr::unnest(ghg_stations) %>%
  # Add the trajectories missing limit interpolation back.
  dplyr::bind_rows(bt_missing_tb) %>%
  # Add trajectory and profile identifiers.
  dplyr::mutate(
    profile_id = stringr::str_c(site, lubridate::date(sample_date), sep = "_"),
    trajectory_id = stringr::str_c(profile_id, end_height, sep = "_"),
    time_to_stations = cross_row
  )



rlog::log_info("Interpolating GHG concentration for backtrajs...")



backtrajectories_tb <-
  backtrajectories_tb %>%
  # Interpolation using 2 observations below and 2 above.
  dplyr::group_by(profile_id) %>%
  dplyr::group_split() %>%
  purrr::map(
    .f = function(data_tb) {
      data_tb[["ghg_updown"]] <-
        moving_window(
          x = data_tb[["ghg_stations"]],
          f = mean,
          hsize = 2,
          na.rm = TRUE
        )
      data_tb[["t2s_updown"]] <-
        moving_window(
          x = data_tb[["time_to_stations"]],
          f = mean,
          hsize = 2,
          na.rm = TRUE
        )
      return(data_tb)
    }
  ) %>%
  # Interpolation using a model.
  purrr::map(
    .f = function(data_tb, min_obs = 4) {
      data_tb["ghg_model"] <- splines_interpolation(
        x = data_tb[["end_height"]],
        y = data_tb[["ghg_stations"]],
        min_obs = 4
      )
      data_tb["t2s_model"] <- splines_interpolation(
        x = data_tb[["end_height"]],
        y = data_tb[["time_to_stations"]],
        min_obs = 4
      )
      return(data_tb)
    }
  ) %>%
  dplyr::bind_rows() %>%
  # Interpolation using past and future profiles (at the same height).
  dplyr::group_by(site, end_height) %>%
  dplyr::arrange(sample_date, .by_group = TRUE) %>%
  dplyr::mutate(
    ghg_bf_1 = dplyr::lag(ghg_stations, n = 1),
    ghg_bf_2 = dplyr::lag(ghg_stations, n = 2),
    ghg_at_1 = dplyr::lead(ghg_stations, n = 1),
    ghg_at_2 = dplyr::lead(ghg_stations, n = 2),
    t2s_bf_1 = dplyr::lag(time_to_stations, n = 1),
    t2s_bf_2 = dplyr::lag(time_to_stations, n = 2),
    t2s_at_1 = dplyr::lead(time_to_stations, n = 1),
    t2s_at_2 = dplyr::lead(time_to_stations, n = 2)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    ghg_bfat_1 = mean(c(ghg_bf_1, ghg_stations, ghg_at_1), na.rm = TRUE),
    ghg_bfat_2 = mean(c(ghg_bf_2, ghg_bf_1, ghg_stations, ghg_at_1, ghg_at_2),
                      na.rm = TRUE),
    t2s_bfat_1 = mean(c(t2s_bf_1, time_to_stations, t2s_at_1), na.rm = TRUE),
    t2s_bfat_2 = mean(c(t2s_bf_2, t2s_bf_1, time_to_stations,
                        t2s_at_1, t2s_at_2), na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    -tidyselect::starts_with("ghg_bf_"),
    -tidyselect::starts_with("ghg_at_"),
    -tidyselect::starts_with("t2s_bf_"),
    -tidyselect::starts_with("t2s_at_")
  )



rlog::log_info("Choosing best GHG concentration from interpolations...")



backtrajectories_tb <-
  backtrajectories_tb %>%
  dplyr::mutate(
    ghg_con = dplyr::coalesce(ghg_stations, ghg_updown, ghg_model,
                              ghg_bfat_1, ghg_bfat_2),
    bt_span = dplyr::coalesce(time_to_stations, t2s_updown, t2s_model,
                              t2s_bfat_1, t2s_bfat_2)
  )



rlog::log_info("Exporting trajectory report...")



report_tb <-
  backtrajectories_tb %>%
  dplyr::select(
    profile_id, trajectory_id, site, sample_date, end_height,
    cross_latitude, cross_date, ghg_stations, ghg_updown, ghg_model,
    ghg_bfat_1, ghg_bfat_2, ghg_concentration = ghg_con,
    time_to_stations, t2s_updown, t2s_model, t2s_bfat_1, t2s_bfat_2,
    backtrajectory_span = bt_span
  ) %>%
  readr::write_csv(file = file.path(out_dir, "report_trajectories.csv"))



rlog::log_info("Exporting profile report...")



report_tb <-
  report_tb %>%
  tidyr::nest(.by = tidyselect::all_of("profile_id"), .key = "data_tb") %>%
  dplyr::mutate(
    n_traj = purrr::map_int(
      .x = data_tb,
      .f = function(data_tb) {
        return(nrow(data_tb))
      }
    ),
    n_miss = purrr::map_int(
      .x = data_tb,
      .f = function(data_tb) {
        return(sum(is.na(data_tb[["ghg_stations"]])))
      }
    ),
    mean_ghg_stations = purrr::map_dbl(
      .x = data_tb,
      .f = function(data_tb) {
        return(mean(data_tb[["ghg_stations"]], na.rm = TRUE))
      }
    ),
    sd_ghg_stations = purrr::map_dbl(
      .x = data_tb,
      .f = function(data_tb) {
        return(sd(data_tb[["ghg_stations"]], na.rm = TRUE))
      }
    ),
    mean_time_to_stations = purrr::map_dbl(
      .x = data_tb,
      .f = function(data_tb) {
        return(mean(data_tb[["time_to_stations"]], na.rm = TRUE))
      }
    ),
    sd_time_to_stations = purrr::map_dbl(
      .x = data_tb,
      .f = function(data_tb) {
        return(sd(data_tb[["time_to_stations"]], na.rm = TRUE))
      }
    )
  ) %>%
  dplyr::select(-data_tb) %>%
  readr::write_csv(file = file.path(out_dir, "report_profiles.csv"))



rlog::log_info("Plotting profiles...")



# Plot by site.
plot_tb <-
  backtrajectories_tb %>%
  tidyr::nest(.by = site, .key = "profile_data") %>%
  dplyr::mutate(
    profile_plot = purrr::map(
      .x = profile_data,
      .f = get_profile_plot,
      cprofileid = "profile_id",
      cghg = "ghg_con",
      cheight = "end_height"
    ),
    out_file = file.path(
      out_dir,
      stringr::str_c("profile_", site, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = profile_plot,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            width = plot_height,
            height = plot_width,
            units = plot_units
          )
      }
    )
  )

# Plot by site-year.
plot_tb <-
  backtrajectories_tb %>%
  dplyr::mutate(y = as.integer(lubridate::year(sample_date))) %>%
  tidyr::nest(.by = tidyselect::all_of(c("site", "y")), .key = "profile_data") %>%
  dplyr::mutate(
    profile_plot = purrr::map(
      .x = profile_data,
      .f = get_profile_plot,
      cprofileid = "profile_id",
      cghg = "ghg_con",
      cheight = "end_height"
    ),
    out_file = file.path(
      out_dir,
      stringr::str_c("profile_", site, "_", y, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = profile_plot,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            width = plot_height,
            height = plot_width,
            units = plot_units
          )
      }
    )
  )

# Plot by site-semester.
plot_tb <-
  backtrajectories_tb %>%
  dplyr::mutate(
    s = YEAR.SEMESTERS[lubridate::month(sample_date)]
  ) %>%
  tidyr::nest(.by = tidyselect::all_of(c("site", "s")),
              .key = "profile_data") %>%
  dplyr::mutate(
    profile_plot = purrr::map(
      .x = profile_data,
      .f = get_profile_plot,
      cprofileid = "profile_id",
      cghg = "ghg_con",
      cheight = "end_height"
    ),
    out_file = file.path(
      out_dir,
      stringr::str_c("profile_", site, "_", s, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = profile_plot,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            width = plot_height,
            height = plot_width,
            units = plot_units
          )
      }
    )
  )

# Plot by site-trimester.
plot_tb <-
  backtrajectories_tb %>%
  dplyr::mutate(
    t = YEAR.TRIMESTERS[lubridate::month(sample_date)]
  ) %>%
  tidyr::nest(.by = tidyselect::all_of(c("site", "t")),
              .key = "profile_data") %>%
  dplyr::mutate(
    profile_plot = purrr::map(
      .x = profile_data,
      .f = get_profile_plot,
      cprofileid = "profile_id",
      cghg = "ghg_con",
      cheight = "end_height"
    ),
    out_file = file.path(
      out_dir,
      stringr::str_c("profile_", site, "_", t, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = profile_plot,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            width = plot_height,
            height = plot_width,
            units = plot_units
          )
      }
    )
  )

# Plot by site-year-semester.
plot_tb <-
  backtrajectories_tb %>%
  dplyr::mutate(
    y = as.integer(lubridate::year(sample_date)),
    s = YEAR.SEMESTERS[lubridate::month(sample_date)]
  ) %>%
  tidyr::nest(.by = tidyselect::all_of(c("site", "y", "s")),
              .key = "profile_data") %>%
  dplyr::mutate(
    profile_plot = purrr::map(
      .x = profile_data,
      .f = get_profile_plot,
      cprofileid = "profile_id",
      cghg = "ghg_con",
      cheight = "end_height"
    ),
    out_file = file.path(
      out_dir,
      stringr::str_c("profile_", site, "_", y, "_", s, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = profile_plot,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            width = plot_height,
            height = plot_width,
            units = plot_units
          )
      }
    )
  )

# Plot by site-year-trimester.
plot_tb <-
  backtrajectories_tb %>%
  dplyr::mutate(
    y = as.integer(lubridate::year(sample_date)),
    t = YEAR.TRIMESTERS[lubridate::month(sample_date)]
  ) %>%
  tidyr::nest(.by = tidyselect::all_of(c("site", "y", "t")),
              .key = "profile_data") %>%
  dplyr::mutate(
    profile_plot = purrr::map(
      .x = profile_data,
      .f = get_profile_plot,
      cprofileid = "profile_id",
      cghg = "ghg_con",
      cheight = "end_height"
    ),
    out_file = file.path(
      out_dir,
      stringr::str_c("profile_", site, "_", y, "_", t, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = profile_plot,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            width = plot_height,
            height = plot_width,
            units = plot_units
          )
      }
    )
  )



rlog::log_info("Plotting trajectory maps...")



# Plot trajectories by site.
plot_tb <-
  backtrajectories_tb %>%
  dplyr::select(profile_id, trajectory_id, site, sample_date, data_df) %>%
  tidyr::unnest(data_df) %>%
  tidyr::nest(.by = site, .key = "map_data") %>%
  dplyr::mutate(
    map_plot = purrr::map(
      .x = map_data,
      .f = get_map_plot,
      cid = "trajectory_id",
      clon = traj_clon,
      clat = traj_clat,
      range_lon = map_lon_range,
      range_lat = map_lat_range
    ),
    out_file = file.path(
      out_dir,
      stringr::str_c("map_", site, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    n_traj = purrr::map_int(
      .x = map_data,
      .f = function(.x) {
        return(length(unique(.x[["trajectory_id"]])))
      }
    )
  ) %>%
  dplyr::mutate(
    map_plot = purrr::map2(
      .x = map_plot,
      .y = n_traj,
      .f = function(map_plot, n_traj) {
        if (n_traj > map_max_traj_leg)
          map_plot <- map_plot + ggplot2::theme(legend.position = "none")
        return(map_plot)
      }
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = map_plot,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            width = plot_height,
            height = plot_width,
            units = plot_units
          )
      }
    )
  )

# Plot trajectories by site-year.
plot_tb <-
  backtrajectories_tb %>%
  dplyr::select(profile_id, trajectory_id, site, sample_date, data_df) %>%
  dplyr::mutate(y = lubridate::year(sample_date)) %>%
  tidyr::unnest(data_df) %>%
  tidyr::nest(.by = tidyselect::all_of(c("site", "y")),
              .key = "map_data") %>%
  dplyr::mutate(
    map_plot = purrr::map(
      .x = map_data,
      .f = get_map_plot,
      cid = "trajectory_id",
      clon = traj_clon,
      clat = traj_clat,
      range_lon = map_lon_range,
      range_lat = map_lat_range
    ),
    out_file = file.path(
      out_dir,
      stringr::str_c("map_", site, "_", y, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    n_traj = purrr::map_int(
      .x = map_data,
      .f = function(.x) {
        return(length(unique(.x[["trajectory_id"]])))
      }
    )
  ) %>%
  dplyr::mutate(
    map_plot = purrr::map2(
      .x = map_plot,
      .y = n_traj,
      .f = function(map_plot, n_traj) {
        if (n_traj > map_max_traj_leg)
          map_plot <- map_plot + ggplot2::theme(legend.position = "none")
        return(map_plot)
      }
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = map_plot,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            width = plot_height,
            height = plot_width,
            units = plot_units
          )
      }
    )
  )

# Plot trajectories by site-semester.
plot_tb <-
  backtrajectories_tb %>%
  dplyr::select(profile_id, trajectory_id, site, sample_date, data_df) %>%
  dplyr::mutate(s = YEAR.SEMESTERS[lubridate::month(sample_date)]) %>%
  tidyr::unnest(data_df) %>%
  tidyr::nest(.by = tidyselect::all_of(c("site", "s")),
              .key = "map_data") %>%
  dplyr::mutate(
    map_plot = purrr::map(
      .x = map_data,
      .f = get_map_plot,
      cid = "trajectory_id",
      clon = traj_clon,
      clat = traj_clat,
      range_lon = map_lon_range,
      range_lat = map_lat_range
    ),
    out_file = file.path(
      out_dir,
      stringr::str_c("map_", site, "_", s, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    n_traj = purrr::map_int(
      .x = map_data,
      .f = function(.x) {
        return(length(unique(.x[["trajectory_id"]])))
      }
    )
  ) %>%
  dplyr::mutate(
    map_plot = purrr::map2(
      .x = map_plot,
      .y = n_traj,
      .f = function(map_plot, n_traj) {
        if (n_traj > map_max_traj_leg)
          map_plot <- map_plot + ggplot2::theme(legend.position = "none")
        return(map_plot)
      }
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = map_plot,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            width = plot_height,
            height = plot_width,
            units = plot_units
          )
      }
    )
  )

# Plot trajectories by site-trimester.
plot_tb <-
  backtrajectories_tb %>%
  dplyr::select(profile_id, trajectory_id, site, sample_date, data_df) %>%
  dplyr::mutate(t = YEAR.TRIMESTERS[lubridate::month(sample_date)]) %>%
  tidyr::unnest(data_df) %>%
  tidyr::nest(.by = tidyselect::all_of(c("site", "t")),
              .key = "map_data") %>%
  dplyr::mutate(
    map_plot = purrr::map(
      .x = map_data,
      .f = get_map_plot,
      cid = "trajectory_id",
      clon = traj_clon,
      clat = traj_clat,
      range_lon = map_lon_range,
      range_lat = map_lat_range
    ),
    out_file = file.path(
      out_dir,
      stringr::str_c("map_", site, "_", t, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    n_traj = purrr::map_int(
      .x = map_data,
      .f = function(.x) {
        return(length(unique(.x[["trajectory_id"]])))
      }
    )
  ) %>%
  dplyr::mutate(
    map_plot = purrr::map2(
      .x = map_plot,
      .y = n_traj,
      .f = function(map_plot, n_traj) {
        if (n_traj > map_max_traj_leg)
          map_plot <- map_plot + ggplot2::theme(legend.position = "none")
        return(map_plot)
      }
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = map_plot,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            width = plot_height,
            height = plot_width,
            units = plot_units
          )
      }
    )
  )



# Plot trajectories by site-year-semester.
plot_tb <-
  backtrajectories_tb %>%
  dplyr::select(profile_id, trajectory_id, site, sample_date, data_df) %>%
  dplyr::mutate(
    y = lubridate::year(sample_date),
    s = YEAR.SEMESTERS[lubridate::month(sample_date)]
  ) %>%
  tidyr::unnest(data_df) %>%
  tidyr::nest(.by = tidyselect::all_of(c("site", "y", "s")),
              .key = "map_data") %>%
  dplyr::mutate(
    map_plot = purrr::map(
      .x = map_data,
      .f = get_map_plot,
      cid = "trajectory_id",
      clon = traj_clon,
      clat = traj_clat,
      range_lon = map_lon_range,
      range_lat = map_lat_range
    ),
    out_file = file.path(
      out_dir,
      stringr::str_c("map_", site, "_", y, "_", s, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    n_traj = purrr::map_int(
      .x = map_data,
      .f = function(.x) {
        return(length(unique(.x[["trajectory_id"]])))
      }
    )
  ) %>%
  dplyr::mutate(
    map_plot = purrr::map2(
      .x = map_plot,
      .y = n_traj,
      .f = function(map_plot, n_traj) {
        if (n_traj > map_max_traj_leg)
          map_plot <- map_plot + ggplot2::theme(legend.position = "none")
        return(map_plot)
      }
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = map_plot,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            width = plot_height,
            height = plot_width,
            units = plot_units
          )
      }
    )
  )



# Plot trajectories by site-year-trimester.
plot_tb <-
  backtrajectories_tb %>%
  dplyr::select(profile_id, trajectory_id, site, sample_date, data_df) %>%
  dplyr::mutate(
    y = lubridate::year(sample_date),
    t = YEAR.TRIMESTERS[lubridate::month(sample_date)]
  ) %>%
  tidyr::unnest(data_df) %>%
  tidyr::nest(.by = tidyselect::all_of(c("site", "y", "t")),
              .key = "map_data") %>%
  dplyr::mutate(
    map_plot = purrr::map(
      .x = map_data,
      .f = get_map_plot,
      cid = "trajectory_id",
      clon = traj_clon,
      clat = traj_clat,
      range_lon = map_lon_range,
      range_lat = map_lat_range
    ),
    out_file = file.path(
      out_dir,
      stringr::str_c("map_", site, "_", y, "_", t, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    n_traj = purrr::map_int(
      .x = map_data,
      .f = function(.x) {
        return(length(unique(.x[["trajectory_id"]])))
      }
    )
  ) %>%
  dplyr::mutate(
    map_plot = purrr::map2(
      .x = map_plot,
      .y = n_traj,
      .f = function(map_plot, n_traj) {
        if (n_traj > map_max_traj_leg)
          map_plot <- map_plot + ggplot2::theme(legend.position = "none")
        return(map_plot)
      }
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = map_plot,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            width = plot_height,
            height = plot_width,
            units = plot_units
          )
      }
    )
  )



rlog::log_info("Finished!")






###############################################################################
# NOTE: Do I need the briefcases' data in this script?
###############################################################################

# briefcase_dir         <- "/home/alber/Documents/data/r_packages/cqmaTools/briefcases"
# stopifnot("Briefcase directory not found" = dir.exists(briefcase_dir))
#
# # Flags to keep in flagcolname.
# keep_flags <- c("...", "..>", "..<")
#
#
# # Column for filtering briefcase data.
# flag_colname <- "flag"
#
#
# stopifnot(c("name", station_clon, station_clat) %in%
#           colnames(stations_lonlat_tb))
#
# # 2 - Read data from the briefcases.
# briefcase_tb <-
#   briefcase_dir %>%
#   list.files(
#     full.names = TRUE,
#     recursive = FALSE,
#     include.dirs = FALSE
#   ) %>%
#   dplyr::as_tibble() %>%
#   dplyr::rename(file_path = "value") %>%
#   dplyr::mutate(file_name = basename(file_path)) %>%
#   tidyr::separate(
#     col = file_name,
#     into = c("site", "gas"),
#     sep = "[.]"
#   ) %>%
#   dplyr::mutate(
#     data_df = purrr::map(
#       file_path,
#       .f = utils::read.table,
#       sep = "",
#       header = FALSE,
#       skip = 0,
#       col.names = BRIEFCASE.COLNAMES
#     )
#   ) %>%
#   # Keep rows with valid flags.
#   dplyr::mutate(
#     data_df = purrr::map(
#       .x = data_df,
#       .f = function(x, flag_colname, keep_flags) {
#         return(x[x[[flag_colname]] %in% keep_flags, ])
#       },
#       flag_colname = flag_colname,
#       keep_flags = keep_flags
#     )
#   ) %>%
#   # Remove duplicated rows.
#   dplyr::mutate(
#     data_df = purrr::map(
#       .x = data_df,
#       .f = dplyr::distinct
#     )
#   ) %>%
#   # Remove longitude & latitude outliers.
#   dplyr::mutate(
#     data_df = purrr::map(
#       .x = data_df,
#       .f = function(x, lon_colname, lat_colname) {
#         stopifnot("Longitude or latitude columns not found!" =
#                     all(c(lon_colname, lat_colname) %in% colnames(x)))
#         return(x[!(is_outlier(x[[lon_colname]]) |
#                      is_outlier(x[[lat_colname]])), ])
#       },
#       lon_colname = station_clon,
#       lat_colname = station_clat
#     )
#   )
