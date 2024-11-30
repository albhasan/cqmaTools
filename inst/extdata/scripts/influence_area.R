library(dplyr)
library(purrr)
library(rlog)
library(stringr)
library(terra)
library(tibble)
library(tidyr)

library(devtools)
load_all()


rlog::log_info("-------------------------------------------------------------")

#---- Configuration ----

rlog::log_info("Reading configuration...")

# Directory with data representing back-trajectories.
traj_dir <-
  "/home/alber/Documents/data/r_packages/cqmaTools/trajectories/2011"

# Directory for storing results.
out_dir <- "/home/alber/Downloads/tmp"

# Trajectory files.
start_row <- 1
end_row <- Inf

# Filter trajectories by height in their filename.
flask_max_height <- 4500

# Filter trajectories by the percentage of their vertices in height range.
min_per_vert_in_hrange <- 0.0

# Filter trajectories' vertices by height.
vert_min_height <- -Inf
vert_max_height <- Inf

# Densify trajectories' segments by a factor of...
densify_n <- 2

# Grid parameters.
grid_resolution <- 1
grid_crs <- 4326
grid_min_lon <- -80
grid_max_lon <- -30
grid_min_lat <- -40
grid_max_lat <- 10

# Raster color palette. See terra::map.pal()
raster_palette <- "reds"
raster_colors <- 100



#---- Validation ----

rlog::log_info("Validating configuration parameters...")

stopifnot("Directory with back-trajectory data not found!" =
            dir.exists(traj_dir))

stopifnot("Result directory not found!" = dir.exists(out_dir))

#---- Utilitary functions ----

#' Utility function for saving a frequeny grid
#'
#' @description
#' Save a frequency grid as a raster file.
#'
#' @param r a raster (terra) object.
#' @param out_file a character(1). Path to the file for storing the given
#' raster.
#'
#' @return a logical. True if the file was created or false otherwise.
#'
save_tif <- function(r, out_file) {
  res <- tryCatch({
    terra::writeRaster(
      x = r,
      filename = out_file,
      gdal = c("COMPRESS=LZW", "TFW=NO", "BIGTIFF=IF_NEEDED"),
      datatype = "INT4S"
    )
    return(TRUE)
  },
  error = function(e) {
    warning(
      sprintf(
        "Unable to write file %s. Messasge: %s",
        out_file,
        conditionMessage(e)
      )
    )
    return(FALSE)
  },
  warning = function(w) {
    message(conditionMessage(w))
    return(FALSE)
  })
  invisible(res)
}

save_plot <- function(grid_r, plot_title, r_range) {
  plot_influence_area(
    r = grid_r,
    r_range = r_range,
    r_col = terra::map.pal(raster_palette, raster_colors),
    x_range = c(grid_min_lon, grid_max_lon),
    y_range = c(grid_min_lat, grid_max_lat),
    add_countries = TRUE,
    ctr_color = "black",
    ctr_lwd = 2.0,
    add_states = TRUE,
    stt_color = "gray",
    stt_lwd = 1.0,
    plot_title = plot_title,
    save_plots = out_dir,
    plot_width = 960,
    plot_height = 960
  )
}



#' Utilify function for computing the frequency grid
#'
#' @description
#' Compute the frequency grid and convert it into a raster.
#'
#' @param .x a data frame.
#'
#' @return a data frame.
#'
comp_freq <- function(.x) {
  fgrid <- compute_frequency_grid(
    traj_df = dplyr::bind_rows(.x[["data"]]),
    grid_sf = grid_sf,
    densify_n = densify_n
  )
  fgrid_r <- grid_to_raster(
    grid_sf = fgrid,
    grid_resolution = grid_resolution,
    cname = "freq"
  )
  return(tibble::tibble(
    site = unique(.x[["site"]]),
    year = unique(.x[["year"]]),
    m_period =  unique(.x[["m_period"]]),
    grid_r = list(fgrid_r)
  ))
}



#' Utility function for computing areas of influence
#'
#' @description
#' This function takes a data frame in which each row represents a
#' back-trajectory and use it to estimate areas of influence.
#'
#' @param data_df a data frame. Each row corresponds to a file and metadata of
#' a back-trajectory.
#' @param var_names a character. Column names in the actual trajectory data
#' used to group area of influence estimation.
#' @param year_peroid a character(12). This represents the way to group months
#' for analysis.
#'
#' @return a data frame with metadata regarding the files produced during
#' analysis.
#'
compute_aois <- function(data_df, var_names, year_period) {
  plot_title <- grid_r <- out_tif <- NULL
  # Group by var_names, compute frequency grid, rasterize, and save.
  data_df <-
    data_df %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(var_names))) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(
      .f = comp_freq
    ) %>%
    tidyr::unite(
      col = "plot_title",
      tidyselect::all_of(var_names),
      sep = "_",
      remove = FALSE
    ) %>%
    dplyr::mutate(out_tif = file.path(out_dir, paste0(plot_title, ".tif"))) %>%
    dplyr::mutate(
      saved = purrr::map2_lgl(
        .x = grid_r,
        .y = out_tif,
        .f = save_tif
      )
    )
  # Minimum and maximum values in the rasters.
  r_range <- get_raster_range(data_df[["grid_r"]])
  # Create plots and save them.
  data_df %>%
    dplyr::mutate(
      plot_file = purrr::map2_chr(
        .x = grid_r,
        .y = plot_title,
        .f = save_plot,
        r_range = r_range
      )
    ) %>%
    return()
}



#---- Script ----

rlog::log_info("Reading back-trajectory metadata...")

traj_tb <-
  traj_dir %>%
  list.files(
    full.names = TRUE,
    pattern = TRAJECTORY.FILENAME.PATTERN,
    recursive = TRUE
  ) %>%
  get_trajectory_metadata() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(site = toupper(site)) %>%
  dplyr::filter(
    height < flask_max_height
  ) %>%
  dplyr::mutate(
    data = purrr::map(
      file_path,
      .f = read_trajectory_file
    )
  ) %>%
  dplyr::mutate(
    data = filter_traj_vertices(
      data,
      from_row = start_row,
      to_row = end_row,
      vert_min_lon = -Inf,
      vert_max_lon = Inf,
      vert_min_lat = -Inf,
      vert_max_lat = Inf,
      vert_min_height = vert_min_height,
      vert_max_height = vert_max_height
    )
  ) %>%
  dplyr::mutate(
    data = filter_traj(
      data,
      traj_min_lon = -Inf,
      traj_max_lon = Inf,
      traj_min_lat = -Inf,
      traj_max_lat = Inf,
      traj_min_height = -Inf,
      traj_max_height = Inf,
      min_per_vert_in_hrange = min_per_vert_in_hrange 
    )
  )

rlog::log_info("Building a grid...")

grid_sf <- build_grid(
  origin_lon = grid_min_lon,
  origin_lat = grid_min_lat,
  min_lon = grid_min_lon,
  max_lon = grid_max_lon,
  min_lat = grid_min_lat,
  max_lat = grid_max_lat,
  grid_resolution = grid_resolution,
  crs = grid_crs
)

rlog::log_info("Processing site-year-trimester...")

year_period <- YEAR.TRIMESTERS
site_year_tri_df <-
  traj_tb %>%
  dplyr::mutate(m_period = year_period[month]) %>%
  compute_aois(
    var_names = c("site", "year", "m_period"),
    year_period = year_period
  )

rlog::log_info("Processing site-year-semester...")

year_period <- YEAR.SEMESTERS
site_year_sem_df <-
  traj_tb %>%
  dplyr::mutate(m_period = year_period[month]) %>%
  compute_aois(
    var_names = c("site", "year", "m_period"),
    year_period = year_period
  )

rlog::log_info("Processing site-year...")

year_period <- YEAR.YEAR
site_year_df <-
  traj_tb %>%
  dplyr::mutate(m_period = year_period[month]) %>%
  compute_aois(
    var_names = c("site", "year", "m_period"),
    year_period = year_period
  )

rlog::log_info("Processing site, all years...")

year_period <- YEAR.YEAR
site_all_years_df <-
  traj_tb %>%
  dplyr::mutate(m_period = year_period[month]) %>%
  compute_aois(
    var_names = c("site"),
    year_period = year_period
  )

rlog::log_info("Processing site-trimester, all years...")

year_period <- YEAR.TRIMESTERS
site_tri_df <-
  traj_tb %>%
  dplyr::mutate(m_period = year_period[month]) %>%
  compute_aois(
    var_names = c("site", "m_period"),
    year_period = year_period
  )

rlog::log_info("Processing site-semester, all years...")

year_period <- YEAR.SEMESTERS
site_sem_df <-
  traj_tb %>%
  dplyr::mutate(m_period = year_period[month]) %>%
  compute_aois(
    var_names = c("site", "m_period"),
    year_period = year_period
  )

rlog::log_info("Finished!")
