library(sf)
library(terra)


library(devtools)
load_all()



#---- Configuration ----
hysplit_path <- "/home/alber/Documents/github/cqmaTools/inst/extdata/trajectories/2011"
out_dir <- "/home/alber/Downloads/tmp"


stopifnot("Hysplit data not found!" = dir.exists(hysplit_path))
stopifnot("Output directory not found!" = dir.exists(out_dir))



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
                                r_range,
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

    plot(
        r, 
        range = r_range,
        xlim = x_range,
        ylim = y_range,
        main = plot_title,
        col = r_col
    )

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

}



#---- Process data ----

# Get the trajectory files.
files_df <- get_trajectory_metadata(hysplit_path)

# Split the trajectories by time periods.
traj_df_ls <- split(
    files_df, 
    f = files_df[c("site", "year", "trimester")]
)

# Util function for processing trajectories from each period.
aoi_fn <- function(x, 
                   vert_min_height, vert_max_height,
                   vert_min_lon, vert_max_lon,
                   vert_min_lat, vert_max_lat) {
    aoi <- compute_frequency_grid(
        files = x[["filepath"]],
        vert_min_height = vert_min_height,
        vert_max_height = vert_max_height,
        vert_min_lon = vert_min_lon,
        vert_max_lon = vert_max_lon,
        vert_min_lat = vert_min_lat,
        vert_max_lat = vert_max_lat
    )
}

# Do the thing.
aoi_ls <- lapply(
    traj_df_ls,
    aoi_fn,
    vert_min_height = 250,
    vert_max_height = 1300,
    vert_min_lon = -80,
    vert_max_lon = -30,
    vert_min_lat = -40,
    vert_max_lat = 10
)

# Save aois rasters to disc.
for (name in names(aoi_ls)) {
    filename <- file.path(out_dir,
       paste0("aoi_", gsub(pattern = "[.]", replacement = "_", name), ".tif"))
    terra::writeRaster(
        overwrite = TRUE,
        aoi_ls[[name]], 
        filename = filename,
        datatype = "INT4S"
    )
}

# Plot
r_range <- range(vapply(aoi_ls, function(x){range(x[], na.rm = TRUE)}, 
                        numeric(2)))

for (pname in names(aoi_ls)) {
    plot_influence_area (
        aoi_ls[[pname]],
        r_range = r_range,
        r_col = terra::map.pal("viridis", 100),
        x_range = c(-80, -30),
        y_range = c(-40, 10),
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
        save_plots = "/home/alber/Downloads/tmp",
        plot_width = 960,
        plot_height = 960
    )
}

