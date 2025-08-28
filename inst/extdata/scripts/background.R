###############################################################################
# BACKGROUND
# This script streamlines the data flow of the CQMA LAB AT INPE
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
# - run alf co and co2 at the same time. co2 produces no output. The control of 
#   the cycle is not only site but site & gas
# - add title to figures including gas name



library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(tools)

library(cqmaTools)



#---- Configuration ----

station_dir           <- "/home/alber/Documents/data/r_packages/cqmaTools/stations/data"
station_location_file <- "/home/alber/Documents/data/r_packages/cqmaTools/stations/station_location.csv"
briefcase_dir         <- "/home/alber/Documents/data/r_packages/cqmaTools/briefcases"
hysplit_dir           <- "/home/alber/Documents/data/r_packages/cqmaTools/trajectories"
out_dir               <- "/home/alber/Downloads/tmp"

# Column for filtering briefcase data.
flag_colname <- "flag"

# Flags to keep in flagcolname.
keep_flags <- c("...", "..>", "..<")

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

# Column names used in the stations.
station_clon <- "lon"
station_clat <- "lat"

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

# Image size for plots (landscape by default).
plot_width  <- 297
plot_height <- 210
plot_units  <- "mm"

# Maximum number of legend items (back trajectories) in plot maps
n_max_bkraj_map_legend <- 20


#---- Load data ----

stations_lonlat_tb <-
  readr::read_csv(station_location_file, col_types = "cdd")

limit_sf <- sf::read_sf(dsn = limit_file)



#---- Validations ----

stopifnot("Directory with stations' data not found!" = dir.exists(station_dir))
stopifnot("Station location file not found!" =
            file.exists(station_location_file))
stopifnot(c("name", station_clon, station_clat) %in% colnames(stations_lonlat_tb))
stopifnot("Briefcase directory not found" = dir.exists(briefcase_dir))
stopifnot("Hysplit directory not found!" = dir.exists(hysplit_dir))
stopifnot("Limit vector not found!" = file.exists(limit_file))
stopifnot("Line expected!" = "LINESTRING" %in% sf::st_geometry_type(limit_sf))
stopifnot("Output directory with not found!" = dir.exists(out_dir))



#---- Utilitary ----

#' Utilitary function to get the lagged observations from 1 to lags.
#' This function ie meant to work with tidyr::across.
#'
#' @param A tibble.
#' @param lags an integer(1). Number of lagged observations.
#'
#' @return A tibble con extra columns.
#'
multi_lag <- function(x, lags) {
  lags <- 1:lags
  names(lags) <- paste0("lag_", as.character(lags))
  purrr::map_dfr(lags, lag, x = x)
}

#' Utilitary function to get the lead observations from 1 to leads.
#' This function ie meant to work with tidyr::across.
#'
#' @param A tibble.
#' @param leads an integer(1). Number of leading observations.
#'
#' @return A tibble con extra columns.
#'
multi_lead <- function(x, leads) {
  leads <- 1:leads
  names(leads) <- paste0("lead_", as.character(leads))
  purrr::map_dfr(leads, lead, x = x)
}

#' Utilitary function for easy plotting
#'
#' @param data_tb A tibble.
#' @param cgroup A character(1). Column name used to group the given data.
#'
#' @return A ggplot2 object.
#'
plot_profile_helper <- function(data_tb, cgroup) {
  data_grouped <- plot_profile <- NULL
  data_tb %>%
    dplyr::mutate(
      plot_profile = purrr::map(
        .x = data_grouped,
        .f = get_profile_plot,
        cprofileid = "profile_id",
        cghg = "ghg_interpolated",
        cheight = "height"
      )
    ) %>%
    dplyr::mutate(
      plot_profile = purrr::map2(
        .x = plot_profile,
        .y = {{cgroup}},
        .f = function(plot_profile, cgroup) {
          plot_profile + ggplot2::ggtitle(cgroup)
        }
      )
    ) %>%
    return()
}

#' Utilitary function for easy plotting
#'
#' @param data_tb A tibble.
#' @param cgroup A character(1). Column name used to group the given data.
#'
#' @return A ggplot2 object.
#'
plot_map_helper <- function(data_tb, cgroup) {
  data_grouped <- plot_map <- NULL
  data_tb %>%
    dplyr::mutate(
      plot_map = purrr::map(
        .x = data_grouped,
        .f = get_map_plot,
        cid = "profile_id",
        clon = traj_clon,
        clat = traj_clat,
        range_lon = map_lon_range,
        range_lat = map_lat_range
      )
    ) %>%
    dplyr::mutate(
      plot_map = purrr::map2(
        .x = plot_map,
        .y = {{cgroup}},
        .f = function(plot_map, cgroup) {
          plot_map <-
            plot_map +
            ggplot2::ggtitle(cgroup) +
            ggplot2::xlab("Longitude") +
            ggplot2::ylab("Latitude")
          return(plot_map)
        }
      )
    ) %>%
    return()
}



#---- Script ----

# 1 - Read data from the metereological stations.
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

# 2 - Read data from the briefcases.
briefcase_tb <-
  briefcase_dir %>%
  list.files(
    full.names = TRUE,
    recursive = FALSE,
    include.dirs = FALSE
  ) %>%
  dplyr::as_tibble() %>%
  dplyr::rename(file_path = "value") %>%
  dplyr::mutate(file_name = basename(file_path)) %>%
  tidyr::separate(
    col = file_name,
    into = c("site", "gas"),
    sep = "[.]"
  ) %>%
  dplyr::mutate(
    data_df = purrr::map(
      file_path,
      .f = utils::read.table,
      sep = "",
      header = FALSE,
      skip = 0,
      col.names = BRIEFCASE.COLNAMES
    )
  ) %>%
  # Keep rows with valid flags.
  dplyr::mutate(
    data_df = purrr::map(
      .x = data_df,
      .f = function(x, flag_colname, keep_flags) {
        return(x[x[[flag_colname]] %in% keep_flags, ])
      },
      flag_colname = flag_colname,
      keep_flags = keep_flags
    )
  ) %>%
  # Remove duplicated rows.
  dplyr::mutate(
    data_df = purrr::map(
      .x = data_df,
      .f = dplyr::distinct
    )
  ) %>%
  # Remove longitude & latitude outliers.
  dplyr::mutate(
    data_df = purrr::map(
      .x = data_df,
      .f = function(x, lon_colname, lat_colname) {
        stopifnot("Longitude or latitude columns not found!" =
                    all(c(lon_colname, lat_colname) %in% colnames(x)))
        return(x[!(is_outlier(x[[lon_colname]]) |
                     is_outlier(x[[lat_colname]])), ])
      },
      lon_colname = station_clon,
      lat_colname = station_clat
    )
  )


# 3 - Read data from the back-trajectories.
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
    into = c("site", "year", "month", "day", "hour", "height"),
    sep = "[_]"
  ) %>%
  tidyr::unite(year, month, day, col = "sample_date", sep = "-") %>%
  dplyr::mutate(
    hour = paste0(hour, ":00:00"),
    height = as.double(height)
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

# 4 - Get of the trajectories whose GHG concentration can't be interpolated
  # from the data in the metereological stations.
bt_missing_tb <-
  backtrajectories_tb %>%
  dplyr::filter(cross_row == 0)

# 5 - Set of gasses observed at the measurement stations.
station_gasses <-
  stations_tb %>%
  dplyr::pull(gas) %>%
  unique() %>%
  sort()

stopifnot("Only one gas is currently supported!" = length(station_gasses) == 1)

# 6 - Estimate the GHG concentrantion each back trajectory carries from the sea
# and interpolte to those backtrajectories that never reach the sea.
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
  # station's measurement. Filter trajectories accordingly.
  dplyr::mutate(
    ghg_estimate = purrr::map2(
      .x = cross_latitude,
      .y = cross_date,
      .f = estimate_ghg,
      stations_tb = stations_tb,
      clat = station_clat,
      cgas = "gas",
      cname = "name",
      cvalue = "value",
      cprefix = "ghg_"
    ),
    ghg_estimate = vector_to_tibble(ghg_estimate)
  ) %>%
  tidyr::unnest(ghg_estimate) %>%
  # Add the trajectories missing limit interpolation back.
  dplyr::bind_rows(bt_missing_tb) %>%
  # Add lagged and lead observations in new columns.
  dplyr::arrange(site, sample_date, height) %>%
  dplyr::group_by(site, height) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = tidyselect::ends_with(match = station_gasses),
      .fns = \(x) multi_lag(x = x, lags = n_obs_interpolation),
      # .fns = multi_lag,
      .unpack = TRUE
      # lags = n_obs_interpolation
    )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = tidyselect::ends_with(match = station_gasses),
      .fns = \(x) multi_lead(x = x, leads = n_obs_interpolation),
      # .fns = multi_lead,
      .unpack = TRUE
      #leads = n_obs_interpolation
    )
  ) %>%
  dplyr::ungroup() %>%
  # Interpolate station data for trajectories that don't cross the limit.
  dplyr::rowwise() %>%
  dplyr::mutate(
    ghg_interpolated =
    mean(
      dplyr::c_across(
        cols = tidyselect::starts_with(
          match = paste0("ghg_", station_gasses, "_", c("lag", "lead"), "_")
        )
      ),
      na.rm = TRUE
    )
  ) %>%
  dplyr::ungroup() %>%
  # Use the existing value when available or replace with the interpolation.
  dplyr::mutate(
    ghg_interpolated = dplyr::if_else(
      condition = rlang::are_na(.data[[paste0("ghg_", station_gasses)]]),
      true = ghg_interpolated,
      false = .data[[paste0("ghg_", station_gasses)]]
    )
  )



# 7 - Plot profiles.
profile_plot_tb <-
  backtrajectories_tb %>%
  dplyr::select(site, sample_date, height, ghg_interpolated) %>%
  dplyr::mutate(
    profile_id = stringr::str_c(site, lubridate::date(sample_date), sep = "_")
  ) %>%
  dplyr::arrange(site, height, sample_date)


# Plot all data.
profile_plot_tb %>%
  get_profile_plot(
    cprofileid = "profile_id",
    cghg = "ghg_interpolated",
    cheight = "height",
    title = ""
  ) %>%
  ggplot2::ggsave(
    filename = file.path(out_dir, "plot_profile.png"),
    width = plot_height,
    height = plot_width,
    units = plot_units
  )

# Plot by year.
profile_plot_tb %>%
  dplyr::mutate(cgroup = lubridate::year(sample_date)) %>%
  tidyr::nest(.by = cgroup, .key = "data_grouped") %>%
  plot_profile_helper(cgroup = cgroup) %>%
  dplyr::mutate(
    out_file = file.path(
      out_dir,
      stringr::str_c("plot_profile_", cgroup, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = plot_profile,
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

# Plot by semester (all years).
profile_plot_tb %>%
  dplyr::mutate(cgroup = YEAR.SEMESTERS[lubridate::month(sample_date)]) %>%
  tidyr::nest(.by = cgroup, .key = "data_grouped") %>%
  plot_profile_helper(cgroup = cgroup) %>%
  dplyr::mutate(
    out_file = file.path(
      out_dir,
      stringr::str_c("plot_profile_", cgroup, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = plot_profile,
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

# Plot by trimester (all years).
profile_plot_tb %>%
  dplyr::mutate(cgroup = YEAR.TRIMESTERS[lubridate::month(sample_date)]) %>%
  tidyr::nest(.by = cgroup, .key = "data_grouped") %>%
  plot_profile_helper(cgroup = cgroup) %>%
  dplyr::mutate(
    out_file = file.path(
      out_dir,
      stringr::str_c("plot_profile_", cgroup, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = plot_profile,
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

# Plot by year-semester.
profile_plot_tb %>%
  dplyr::mutate(
    cgroup = stringr::str_c(
      lubridate::year(sample_date),
      YEAR.SEMESTERS[lubridate::month(sample_date)],
      sep = "_"
    )
  ) %>%
  tidyr::nest(.by = cgroup, .key = "data_grouped") %>%
  plot_profile_helper(cgroup = cgroup) %>%
  dplyr::mutate(
    out_file = file.path(
      out_dir,
      stringr::str_c("plot_profile_", cgroup, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = plot_profile,
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

# Plot by year-trimester
profile_plot_tb %>%
  dplyr::mutate(
    cgroup = stringr::str_c(
      lubridate::year(sample_date),
      YEAR.TRIMESTERS[lubridate::month(sample_date)],
      sep = "_"
    )
  ) %>%
  tidyr::nest(.by = cgroup, .key = "data_grouped") %>%
  plot_profile_helper(cgroup = cgroup) %>%
  dplyr::mutate(
    out_file = file.path(
      out_dir,
      stringr::str_c("plot_profile_", cgroup, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = plot_profile,
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

# 8 - Plot trajectory maps.
trajectory_plot_tb <-
  backtrajectories_tb %>%
  dplyr::select(site, sample_date, final_height = height, data_df) %>%
  dplyr::mutate(
    profile_id = paste(site, lubridate::date(sample_date), final_height,
                       sep = "_")
  ) %>%
  tidyr::unnest(data_df) %>%
  dplyr::arrange(site, sample_date)

# Plot map of all the trajectories.
(
  trajectory_plot_tb %>%
    get_map_plot(
      cid = "profile_id",
      clon = traj_clon,
      clat = traj_clat,
      range_lon = map_lon_range,
      range_lat = map_lat_range
    ) +
    ggplot2::theme(legend.position = "none")
) %>%
  ggplot2::ggsave(
    filename = file.path(out_dir, "plot_map.png"),
    height = plot_height,
    width = plot_width,
    units = plot_units
  )

# Plot by year.
trajectory_plot_tb %>%
  dplyr::mutate(cgroup = lubridate::year(sample_date)) %>%
  tidyr::nest(.by = cgroup, .key = "data_grouped") %>%
  dplyr::mutate(n_bktraj = purrr::map_int(
    .x = data_grouped,
    .f = function(.x) {
      as.integer(length(unique(.x[["profile_id"]])))
    }
  )) %>%
  plot_map_helper(cgroup = cgroup) %>%
  dplyr::mutate(
    plot_map = purrr::map2(
      .x = n_bktraj,
      .y = plot_map,
      .f = function(.x, .y) {
        if (.x > n_max_bkraj_map_legend) {
          return(.y + ggplot2::theme(legend.position = "none"))
        }
        return(.y)
      }
    )
  ) %>%
  dplyr::mutate(
    out_file = file.path(
      out_dir,
      stringr::str_c("plot_map_", cgroup, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = plot_map,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            height = plot_height,
            width = plot_width,
            units = plot_units
          )
      }
    )
  )


# Plot by semester.
trajectory_plot_tb %>%
  dplyr::mutate(cgroup = YEAR.SEMESTERS[lubridate::month(sample_date)]) %>%
  tidyr::nest(.by = cgroup, .key = "data_grouped") %>%
  dplyr::mutate(n_bktraj = purrr::map_int(
    .x = data_grouped,
    .f = function(.x) {
      as.integer(length(unique(.x[["profile_id"]])))
    }
  )) %>%
  plot_map_helper(cgroup = cgroup) %>%
  dplyr::mutate(
    plot_map = purrr::map2(
      .x = n_bktraj,
      .y = plot_map,
      .f = function(.x, .y) {
        if (.x > n_max_bkraj_map_legend) {
          return(.y + ggplot2::theme(legend.position = "none"))
        }
        return(.y)
      }
    )
  ) %>%
  dplyr::mutate(
    out_file = file.path(
      out_dir,
      stringr::str_c("plot_map_", cgroup, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = plot_map,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            height = plot_height,
            width = plot_width,
            units = plot_units
          )
      }
    )
  )


# Plot by trimester.
trajectory_plot_tb %>%
  dplyr::mutate(cgroup = YEAR.TRIMESTERS[lubridate::month(sample_date)]) %>%
  tidyr::nest(.by = cgroup, .key = "data_grouped") %>%
  dplyr::mutate(n_bktraj = purrr::map_int(
    .x = data_grouped,
    .f = function(.x) {
      as.integer(length(unique(.x[["profile_id"]])))
    }
  )) %>%
  plot_map_helper(cgroup = cgroup) %>%
  dplyr::mutate(
    plot_map = purrr::map2(
      .x = n_bktraj,
      .y = plot_map,
      .f = function(.x, .y) {
        if (.x > n_max_bkraj_map_legend) {
          return(.y + ggplot2::theme(legend.position = "none"))
        }
        return(.y)
      }
    )
  ) %>%
  dplyr::mutate(
    out_file = file.path(
      out_dir,
      stringr::str_c("plot_map_", cgroup, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = plot_map,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            height = plot_height,
            width = plot_width,
            units = plot_units
          )
      }
    )
  )


# Plot by year-semester.
trajectory_plot_tb %>%
  dplyr::mutate(
    cgroup = stringr::str_c(
      lubridate::year(sample_date),
      YEAR.SEMESTERS[lubridate::month(sample_date)],
      sep = "_"
    )
  ) %>%
  tidyr::nest(.by = cgroup, .key = "data_grouped") %>%
  dplyr::mutate(n_bktraj = purrr::map_int(
    .x = data_grouped,
    .f = function(.x) {
      as.integer(length(unique(.x[["profile_id"]])))
    }
  )) %>%
  plot_map_helper(cgroup = cgroup) %>%
  dplyr::mutate(
    plot_map = purrr::map2(
      .x = n_bktraj,
      .y = plot_map,
      .f = function(.x, .y) {
        if (.x > n_max_bkraj_map_legend) {
          return(.y + ggplot2::theme(legend.position = "none"))
        }
        return(.y)
      }
    )
  ) %>%
  dplyr::mutate(
    out_file = file.path(
      out_dir,
      stringr::str_c("plot_map_", cgroup, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = plot_map,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            height = plot_height,
            width = plot_width,
            units = plot_units
          )
      }
    )
  )


# Plot by year-trimester.
trajectory_plot_tb %>%
  dplyr::mutate(
    cgroup = stringr::str_c(
      lubridate::year(sample_date),
      YEAR.TRIMESTERS[lubridate::month(sample_date)],
      sep = "_"
    )
  ) %>%
  tidyr::nest(.by = cgroup, .key = "data_grouped") %>%
  dplyr::mutate(n_bktraj = purrr::map_int(
    .x = data_grouped,
    .f = function(.x) {
      as.integer(length(unique(.x[["profile_id"]])))
    }
  )) %>%
  plot_map_helper(cgroup = cgroup) %>%
  dplyr::mutate(
    plot_map = purrr::map2(
      .x = n_bktraj,
      .y = plot_map,
      .f = function(.x, .y) {
        if (.x > n_max_bkraj_map_legend) {
          return(.y + ggplot2::theme(legend.position = "none"))
        }
        return(.y)
      }
    )
  ) %>%
  dplyr::mutate(
    out_file = file.path(
      out_dir,
      stringr::str_c("plot_map_", cgroup, ".png", sep = "")
    )
  ) %>%
  dplyr::mutate(
    out_file = purrr::map2(
      .x = out_file,
      .y = plot_map,
      .f = function(.x, .y) {
        .y %>%
          ggplot2::ggsave(
            filename = .x,
            height = plot_height,
            width = plot_width,
            units = plot_units
          )
      }
    )
  )
