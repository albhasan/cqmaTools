test_that("compute_frequency_grid works", {

  files <- list.files(
    path = system.file("extdata", "backtrajectories", package = "cqmaTools"),
    pattern = "^[A-Z]{3}_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]+.[0-9]+",
    full.names = TRUE
  )
  if (length(files) > 10)
    files <- files[sample(seq_along(files))[1:10]]

  df_ls <- files2df(
    files = files,
    header = FALSE,
    skip = 7,
    cnames = HYSPLIT.COLNAMES
  )

  traj_df <- do.call(rbind, df_ls)

  lon_o <- -180
  lat_o <- -90
  lon_min <- -180
  lon_max <- 180
  lat_min <- -90
  lat_max <- 90
  res <- 10
  srs <- 4326
  grid_sf <- build_grid(
    origin_lon = lon_o, origin_lat = lat_o,
    min_lon = lon_min, max_lon = lon_max,
    min_lat = lat_min, max_lat = lat_max,
    grid_resolution = res, crs = 4326
  )

  grid_freq <- compute_frequency_grid(
    traj_df,
    grid_sf,
    clon = "lon",
    clat = "lat",
    cheight = "height",
    grid_id = "grid_id",
    crs = sf::st_crs(grid_sf)
  )

  expect_true("freq" %in% colnames(grid_freq))

  # NOTE: Trajectory vertex on cell borders could be counted more than once.
  expect_true(sum(grid_freq[["freq"]], na.rm = TRUE) >= nrow(traj_df))

})
