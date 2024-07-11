library(devtools)
load_all()



#---- Configuration ----

hysplit_path <- "/home/alber/Documents/github/cqmaTools/inst/extdata/trajectories/2011"
out_dir <- "/home/alber/Downloads/tmp"

stopifnot("Hysplit data not found!" = dir.exists(hysplit_path))
stopifnot("Output directory not found!" = dir.exists(out_dir))

# Grid parameters.
grid_resolution <- 1
grid_crs <- 4326
grid_min_lon = -80
grid_max_lon = -30
grid_min_lat = -40
grid_max_lat = 10

# Filter trajectories by height in their filename.
flask_max_height <- 1300

# Filter trajectories' vertices by height.
vert_min_height <- -Inf
vert_max_height <- 1300

# Filter trajectories by the percentage of their vertices in height range.
min_per_vert_in_hrange <- 0.0

# Trajectory files.
skip = 7
start_row = 1
end_row = 48



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
aoi_fn <- function(x, grid_sf, skip, from_row, to_row, vert_min_height,
                   vert_max_height, vert_min_lon, vert_max_lon, vert_min_lat,
                   vert_max_lat, min_per_vert_in_hrange) {
    return(compute_frequency_grid(files = x[["filepath"]], grid_sf = grid_sf,
        skip = skip, from_row = from_row, to_row = to_row,
        vert_min_height = vert_min_height, vert_max_height = vert_max_height,
        vert_min_lon = vert_min_lon, vert_max_lon = vert_max_lon,
        vert_min_lat = vert_min_lat, vert_max_lat = vert_max_lat,
        min_per_vert_in_hrange = min_per_vert_in_hrange))
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
process_period <- function(m_period, split_by) {

    files_df <- get_trajectory_metadata(files = files, m_period = m_period,
        cnames = TRAJECTORY.COLNAMES)

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
    aoi_ls <- lapply( traj_df_ls, aoi_fn, grid_sf = grid_sf, skip = skip,
        from_row = start_row, to_row = end_row,
        vert_min_height = vert_min_height, vert_max_height = vert_max_height,
        vert_min_lon = grid_min_lon, vert_max_lon = grid_max_lon,
        vert_min_lat = grid_min_lat, vert_max_lat = grid_max_lat, 
        min_per_vert_in_hrange = min_per_vert_in_hrange)

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
            x_range = c(grid_min_lon, grid_max_lon),
            y_range = c(grid_min_lat, grid_max_lat),
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
pfiles <- character(0)
for (m_period in list(YEAR.TRIMESTERS, YEAR.SEMESTERS, YEAR.YEAR)) {
    m_period <- unlist(m_period)
    pn <- process_period(m_period, split_by = c("site", "year", "m_period"))
    pfiles <- append(pfiles, pn)
}
print("-------------------------------------------------------------------")
print("Plotting yearly AOIs by trimester, semester, and whole year...")
print(pfiles)
print("-------------------------------------------------------------------")

# Compute areas of influence by sub-yearly periods, all years.
pfiles <- character(0)
for (m_period in list(YEAR.TRIMESTERS, YEAR.SEMESTERS)) {
    m_period <- unlist(m_period)
    pn <- process_period(m_period, split_by = c("site", "m_period"))
    pfiles <- append(pfiles, pn)
}
print("-------------------------------------------------------------------")
print("Plotting all-years AOIs by trimester, and semester...")
print(pfiles)
print("-------------------------------------------------------------------")

# Compute areas of influence by site, all years.
pfiles <- character(0)
for (m_period in list(YEAR.YEAR)) {
    m_period <- unlist(m_period)
    pn <- process_period(m_period, split_by = "site")
    pfiles <- append(pfiles, pn)
}
print("-------------------------------------------------------------------")
print("Plotting AOIs by site...")
print(pfiles)
print("-------------------------------------------------------------------")

