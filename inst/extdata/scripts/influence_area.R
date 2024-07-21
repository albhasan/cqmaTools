library(devtools)
load_all()



#---- Configuration ----

hysplit_path <- "/home/alber/Documents/github/cqmaTools/inst/extdata/trajectories/2011"
out_dir <- "/home/alber/Downloads/tmp"

stopifnot("Hysplit data not found!" = dir.exists(hysplit_path))
stopifnot("Output directory not found!" = dir.exists(out_dir))

# Grid parameters.
grid_resolution <- 1
grid_crs     <- 4326
grid_min_lon <- -80
grid_max_lon <- -30
grid_min_lat <- -40
grid_max_lat <- 10
grid_id      <- "grid_id"

# Filter trajectories by height in their filename.
flask_max_height <- 1300

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

# Filter trajectories by the percentage of their vertices in height range.
min_per_vert_in_hrange <- 0.0

# Trajectory files.
skip      <- 7
start_row <- 1
end_row   <- 48

# Plot parameters.
plot_min_lon <- -80
plot_max_lon <- -30
plot_min_lat <- -40
plot_max_lat <- 10

# Column names.
clon    <- "lon"
clat    <- "lat"
cheight <- "height"

#---- Load code ----

# Install/load packages.
required_packages <- c("sf", "terra")
if(any(!(required_packages %in% installed.packages()))) {
    install.packages(required_packages[!required_packages %in%
                                       installed.packages()],
                     dependencies = TRUE)}
sapply(required_packages, require, character.only = TRUE)
rm(required_packages)



#---- Utility functions ----

# TODO: Convert into package function. Save map data as part of the package.
plot_influence_area <- function(r,
                                r_range = range(r[]),
                                r_col = terra::map.pal("viridis", 100),
                                x_range = c(-180, 180),
                                y_range = c(-90, 90),
                                add_countries = TRUE,
                                ctr_color = "black",
                                ctr_lwd = 2.0,
                                add_states = TRUE, 
                                stt_color = "gray",
                                stt_lwd = 1.0,
                                add_biomes = TRUE,
                                bms_color = "green",
                                bms_lwd = 0.5, 
                                plot_title = "",
                                save_plots = FALSE,
                                plot_width = 480,
                                plot_height = 480) {

    plot_fname <- NA
    plot2file <- FALSE
    if (save_plots == TRUE)
        save_plots <- getwd()

    if (is.character(save_plots)) 
        if (dir.exists(save_plots))
            plot2file <- TRUE

    if (plot2file) {
        plot_fname <- file.path(out_dir, paste0( "plot_aoi_",
            gsub(pattern = "[.]", replacement = "_", plot_title), ".png"))
        grDevices::png(
            filename = plot_fname, 
            width = plot_width, 
            height = plot_height
        )
    }

    plot(r, range = r_range, xlim = x_range, ylim = y_range,
         main = plot_title, col = r_col)

    if (add_biomes) {
        biomes_br <- geobr::read_biomes()
        biomes_br <- sf::st_transform(biomes_br, crs = terra::crs(r))
        plot(biomes_br[["geom"]], border = bms_color, lwd = bms_lwd,
             type = "l", add = TRUE)
    }

    if (add_states) {
        states_br <- geobr::read_state()
        states_br <- sf::st_transform(states_br, crs = terra::crs(r))
        plot(states_br[["geom"]], border = stt_color, lwd = stt_lwd, 
             type = "l", add = TRUE)
    }

    if (add_countries) 
        maps::map("world", lwd = ctr_lwd, col = ctr_color, add = TRUE)

    if (plot2file)
        dev.off()

    invisible(plot_fname)

}



# Util function for processing trajectories from each period.
aoi_fn <- function(x, grid_sf, skip,
                   from_row, to_row,
                   clon, clat, cheight,
                   vert_min_height,
                   vert_max_height, vert_min_lon, vert_max_lon, vert_min_lat,
                   vert_max_lat, min_per_vert_in_hrange,
                   traj_min_height, traj_max_height,
                   traj_min_lon, traj_max_lon,
                   traj_min_lat, traj_max_lat,
                   grid_id, cnames, hysplit_cnames) {

    # Read trajectory files into a data frames
    data_df_ls <- files2df(files = x[["filepath"]],
                           header = FALSE,
                           skip = skip,
                           cnames = hysplit_cnames)

    # Filter trajectories.
    traj_ls <- filter_traj(data_df_ls, from_row = from_row, to_row = to_row,
        clon = clon, clat = clat, cheight = cheight,
        traj_min_lon = traj_min_lon, traj_max_lon = traj_max_lon,
        traj_min_lat = traj_min_lat, traj_max_lat = traj_max_lat,
        traj_min_height = traj_min_height, traj_max_height = traj_max_height,
        vert_min_lon = vert_min_lon, vert_max_lon = vert_max_lon,
        vert_min_lat = vert_min_lat, vert_max_lat = vert_max_lat,
        vert_min_height = vert_min_height, vert_max_height = vert_max_height,
        min_per_vert_in_hrange = min_per_vert_in_hrange)

    # Bind data frames into one.
    traj_df <- do.call(rbind, traj_ls)
    if (nrow(traj_df) == 0) {
        warning("Empty data frame!")
        return(NA)
    }

    freq_grid <- compute_frequency_grid(traj_df, grid_sf = grid_sf,
                                   clon = clon, clat = clat, cheight = cheight,
                                   grid_id = grid_id)

    if (is.na(freq_grid))
        warning("Empty frequency grid!")

    return(freq_grid)
}



#---- Process data ----



# Build a grid.
grid_sf <- build_grid(origin_lon = grid_min_lon, origin_lat = grid_min_lat,
                      min_lon = grid_min_lon, max_lon = grid_max_lon,
                      min_lat = grid_min_lat, max_lat = grid_max_lat,
                      grid_resolution = grid_resolution, crs = grid_crs)

# Get a data frame of trajectory files.
files <- list.files(path = hysplit_path, pattern = TRAJECTORY.FILENAME.PATTERN,
                    full.names = TRUE, recursive = TRUE)



#---- Process by time period ----



# Helper function for processing areas of influence by different time periods.

process_season_traj <- function(m_period, split_by, files,
                                clon, clat, cheight,
                                cnames, hysplit_cnames, grid_id) {

    files_df <- get_trajectory_metadata(files = files, m_period = m_period,
                                        cnames = cnames)

    if ("site" %in% split_by)
        files_df["site"] <- toupper(files_df[["site"]])

    # Remove trajectories above certain height in their filenames.
    files_df <- files_df[files_df[["height"]] <= flask_max_height,]

    # Split the trajectories by time periods (e.g. trimestres).
    traj_df_ls <- split(files_df, f = files_df[split_by])

    # Remove time periods without trajectories.
    n_trajs <- vapply(traj_df_ls, nrow, integer(1))
    traj_df_ls <- traj_df_ls[n_trajs > 0]

    # Do the thing.
    aoi_ls <- lapply(
        traj_df_ls, aoi_fn, grid_sf = grid_sf, skip = skip,
        from_row = start_row, to_row = end_row,
        clon = clon, clat = clat, cheight = cheight,
        vert_min_height = vert_min_height, vert_max_height = vert_max_height,
        vert_min_lon = vert_min_lon, vert_max_lon = vert_max_lon,
        vert_min_lat = vert_min_lat, vert_max_lat = vert_max_lat,
        min_per_vert_in_hrange = min_per_vert_in_hrange,
        traj_min_height = traj_min_height, traj_max_height = traj_max_height,
        traj_min_lon = traj_min_lon, traj_max_lon = traj_max_lon,
        traj_min_lat = traj_min_lat, traj_max_lat = traj_max_lat,
        cnames = cnames, hysplit_cnames = hysplit_cnames,
        grid_id = grid_id
    )

    # Cast grids to rasters.
    aoi_ls <- lapply(aoi_ls, FUN = grid_to_raster,
        grid_resolution = grid_resolution, cname = "freq")

    # Save aois rasters to disc.
    for (name in names(aoi_ls)) {
        filename <- file.path(out_dir,
            paste0("aoi_", gsub(pattern = "[.]", replacement = "_", name),
                ".tif"))
        terra::writeRaster(overwrite = TRUE, aoi_ls[[name]],
            filename = filename, datatype = "INT4S")
    }

    # Plot
    r_range <- range(vapply(aoi_ls, function(x){range(x[], na.rm = TRUE)}, 
        numeric(2)))

    plot_files <- ""
    for (pname in names(aoi_ls)) {
        p_file <- plot_influence_area (
            aoi_ls[[pname]],
            r_range = r_range,
            r_col = terra::map.pal("viridis", 100),
            x_range = c(plot_min_lon, plot_max_lon),
            y_range = c(plot_min_lat, plot_max_lat),
            add_countries = TRUE,
            ctr_color = "black",
            ctr_lwd = 2.0,
            add_states = TRUE, 
            stt_color = "gray",
            stt_lwd = 1.0,
            add_biomes = TRUE,
            bms_color = "green",
            bms_lwd = 0.5, 
            plot_title = pname,
            save_plots = out_dir,
            plot_width = 960,
            plot_height = 960
        )
        plot_files <- append(plot_files, p_file)
    }

    return(plot_files)

}

# Compute areas of influence by year and sub-yearly periods.
plot_files <- character(0)
for (m_period in list(YEAR.TRIMESTERS, YEAR.SEMESTERS, YEAR.YEAR)) {
    m_period <- unlist(m_period)
    pn <- process_season_traj(
        m_period, split_by = c("site", "year", "m_period"), files = files,
        clon = clon, clat = clat, cheight = cheight,
        cnames = TRAJECTORY.COLNAMES, hysplit_cnames = HYSPLIT.COLNAMES,
        grid_id = grid_id
    )
    plot_files <- append(plot_files, pn)
}
print("-------------------------------------------------------------------")
print("Plotting yearly AOIs by trimester, semester, and whole year...")
print(plot_files)
print("-------------------------------------------------------------------")

# Compute areas of influence by sub-yearly periods, all years.
plot_files <- character(0)
for (m_period in list(YEAR.TRIMESTERS, YEAR.SEMESTERS)) {
    m_period <- unlist(m_period)
    pn <- process_season_traj(
        m_period, split_by = c("site", "m_period"), files = files,
        clon = clon, clat = clat, cheight = cheight,
        cnames = TRAJECTORY.COLNAMES, hysplit_cnames = HYSPLIT.COLNAMES,
        grid_id = grid_id
    )
    plot_files <- append(plot_files, pn)
}
print("-------------------------------------------------------------------")
print("Plotting all-years AOIs by trimester, and semester...")
print(plot_files)
print("-------------------------------------------------------------------")

# Compute areas of influence by site, all years.
plot_files <- character(0)
for (m_period in list(YEAR.YEAR)) {
    m_period <- unlist(m_period)
    pn <- process_season_traj(
        m_period, split_by = "site",files = files,
        clon = clon, clat = clat, cheight = cheight,
        cnames = TRAJECTORY.COLNAMES, hysplit_cnames = HYSPLIT.COLNAMES,
        grid_id = grid_id
    )
    plot_files <- append(plot_files, pn)
}
print("-------------------------------------------------------------------")
print("Plotting AOIs by site...")
print(plot_files)
print("-------------------------------------------------------------------")

