library(dplyr)
library(maps)
library(purrr)
library(sf)

sf::sf_use_s2(use_s2 = TRUE)

devtools::load_all()

# Spatial data.

tolerance_mts <- 10000
#tolerance_deg <- (tolerance_mts / 6378137) * (180 / pi)

countries_sf <- rnaturalearth::ne_countries(scale = "small")
countries_sf <- countries_sf["name"]
countries_sf <- sf::st_make_valid(countries_sf)
countries_sf <- countries_sf[sf::st_is_valid(countries_sf), ]
countries_sf <- sf::st_simplify(
  x = countries_sf,
  preserveTopology = TRUE,
  dTolerance = tolerance_mts
)
stopifnot(all(sf::st_is_valid(countries_sf)))

states_sf <- rnaturalearth::ne_states()
states_sf <- states_sf["name"]
states_sf <- sf::st_make_valid(states_sf)
stopifnot(sf::st_is_valid(states_sf))
states_sf <- sf::st_simplify(
  x = states_sf,
  preserveTopology = TRUE,
  dTolerance = tolerance_mts
)
stopifnot(sf::st_is_valid(states_sf))


usethis::use_data(countries_sf, states_sf, overwrite = TRUE)


# Backtrajectories, briefcases, rawdata.

# NOTE: All of the profiles with data for backtrajectories, briefcases, and 
#       rawdata.
# common_pids <- c(
#   "ALF_2011-01-24", "ALF_2011-03-01", "ALF_2011-03-25", "ALF_2011-04-16",
#   "ALF_2011-05-17", "ALF_2011-06-19", "ALF_2011-06-28", "ALF_2011-07-20",
#   "ALF_2011-07-30", "ALF_2011-08-23", "ALF_2011-08-31", "ALF_2011-09-17",
#   "ALF_2011-09-28", "ALF_2011-10-24", "ALF_2011-11-02", "ALF_2011-11-27",
#   "ALF_2011-12-18", "ALF_2011-12-30"
# )

common_pids <- c(
  "ALF_2011-01-24", "ALF_2011-03-01",  "ALF_2011-04-16",
  "ALF_2011-05-17", "ALF_2011-06-19",  "ALF_2011-07-20",
  "ALF_2011-08-23", "ALF_2011-09-17",
  "ALF_2011-10-24", "ALF_2011-11-02",
  "ALF_2011-12-18"
)

out_dir <- "/home/alber/Documents/github/cqmaTools/inst/extdata"

backtrajectory_dir <-
  "/home/alber/Documents/data/r_packages/cqmaTools/trajectories"

backtrajectory_tb <-
  backtrajectory_dir %>%
  list.files(
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE,
    pattern = TRAJECTORY.FILENAME.PATTERN
  ) %>%
  (function(x) {
    stopifnot("No backtrajectory files found!" = length(x) > 0)
    return(x)
  }) %>%
  dplyr::as_tibble() %>%
  dplyr::rename(file_path = "value") %>%
  dplyr::mutate(file_name = basename(file_path)) %>%
  tidyr::separate(
    col = file_name,
    into = c("site", "year", "month", "day", "hour", "end_height"),
    sep = "[_]"
  ) %>%
  tidyr::unite(year, month, day, col = "sample_date", sep = "-") %>%
  dplyr::mutate(profile_id = stringr::str_c(site, sample_date, sep = "_")) %>%
  dplyr::filter(profile_id %in% common_pids) %>%
  dplyr::mutate(
    file_out = file.path(out_dir, "backtrajectories", basename(file_path))
  ) %>%
  dplyr::select(from = file_path, to = file_out) %>%
  dplyr::mutate(
    cp_res = purrr::map2_lgl(
      .x = from,
      .y = to,
      .f = file.copy,
      overwrite = FALSE
    )
  ) %>%
  (function(x) {
    stopifnot("Couldn't copy backtrajectory files!" = all(x[["cp_res"]]))
    return(x)
  })

rawdata_dir <- "/home/alber/Documents/data/r_packages/cqmaTools/rawdata"

devtools::load_all()

rawdata_tb <-
  rawdata_dir %>%
  list.files(
    full.names = TRUE,
    recursive = FALSE,
    include.dirs = FALSE
  ) %>%
  (function(x) {
    stopifnot("No rawdata files found!" = length(x) > 0)
    return(x)
  }) %>%
  dplyr::as_tibble() %>%
  dplyr::rename(file_path = "value") %>%
  dplyr::mutate(file_name = basename(file_path)) %>%
  tidyr::separate(
    col = file_name,
    into = c(NA, "gas"),
    sep = "[.]"
  ) %>%
  dplyr::mutate(
    data_df = purrr::map(
      .x = file_path,
      .f = utils::read.table,
      sep = "",
      header = FALSE,
      skip = RAWDATA.SKIP,
      col.names = names(RAWDATA.COLNAMES),
      colClasses = rep(x = "character", times = length(RAWDATA.COLNAMES))
    )
  ) %>%
  # Filter data by profile.
  dplyr::mutate(
    data_df = purrr::map(
      .x = data_df,
      .f = function(data_df) {
        date_chr <- paste(
          data_df[["year"]],
          sprintf("%02d", as.integer(data_df[["month"]])),
          sprintf("%02d", as.integer(data_df[["day"]])),
          sep = "-"
        )
        profile_id <- paste(
          data_df[["site"]],
          lubridate::as_date(date_chr),
          sep = "_"
        )
        return(data_df[profile_id %in% common_pids, ])
      }
    )
  ) %>%
  # Count the number of rows.
  dplyr::mutate(
    n_rows = purrr::map_int(
      .x = data_df,
      .f = nrow
    )
  ) %>%
  dplyr::filter(n_rows > 0) %>%
  # Buld out_dir
  dplyr::mutate(
    file_out = file.path(out_dir, "rawdata", basename(file_path))
  ) %>%
  dplyr::mutate(
    wt = purrr::map2_lgl(
      .x = data_df,
      .y = file_out,
      .f = function(data_df, file_out) {
        utils::write.table(
          x = data_df,
          file = file_out,
          append = FALSE,
          quote = FALSE,
          sep = " ",
          dec = ".",
          #col.names = FALSE,
          row.names = FALSE,
          col.names = FALSE
        )
        return(TRUE)
      }
    )
  )

briefcase_dir <- "/home/alber/Documents/data/r_packages/cqmaTools/briefcases"

briefcase_tb <-
  briefcase_dir %>%
  list.files(
    full.names = TRUE,
    recursive = FALSE,
    include.dirs = FALSE
  ) %>%
  (function(x) {
    stopifnot("No briefcase files found!" = length(x) > 0)
    return(x)
  }) %>%
  dplyr::as_tibble() %>%
  dplyr::rename(file_path = "value") %>%
  dplyr::mutate(
    data_df = purrr::map(
      .x = file_path,
      .f = utils::read.table,
      sep = " ",
      header = TRUE,
      #col.names = names(BRIEFCASE.COLNAMES),
      #colClasses = rep("character", times = length(BRIEFCASE.COLNAMES)),
      skip = BRIEFCASE.SKIP,
      tryLogical = FALSE,
      check.names = FALSE
    )
  ) %>%
  dplyr::mutate(
    data_df = purrr::map(
      .x = data_df,
      .f = function(data_df) {
        pid1 <- substr(x = data_df[["profile"]], start = 1L, stop = 4L)
        pid2 <- substr(x = data_df[["profile"]], start = 5L, stop = 1000L)
        pid2 <- gsub(pattern = "_", replacement = "-", x = pid2)
        profile_id <- paste0(pid1, pid2)
        return(data_df[profile_id %in% common_pids, ])
      }
    )
  ) %>%
  # Count the number of rows.
  dplyr::mutate(
    n_rows = purrr::map_int(
      .x = data_df,
      .f = nrow
    )
  ) %>%
  dplyr::filter(n_rows > 0) %>%
  # Buld out_dir
  dplyr::mutate(
    file_out = file.path(out_dir, "briefcases", basename(file_path))
  ) %>%
  dplyr::mutate(
    wt = purrr::map2_lgl(
      .x = data_df,
      .y = file_out,
      .f = function(data_df, file_out) {
        utils::write.table(
          x = data_df,
          file = file_out,
          append = FALSE,
          quote = TRUE,
          sep = " ",
          dec = ".",
          #col.names = FALSE,
          row.names = TRUE,
          col.names = TRUE
        )
        return(TRUE)
      }
    )
  )
