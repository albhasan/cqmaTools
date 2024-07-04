library(cqma)



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




#---- Utility funcitons ----



#---- Process data ----


# Get the trajectory files.
files_df <- get_trajectory_metadata(hysplit_path)

# Split the trajectories by time periods.
files_df_ls <- split(
    files_df, 
    f = files_df[c("site", "year", "trimester")]
)

# Util function for processing trajectories from each period.
aoi_fn <- function(x, vert_min_height, vert_max_height) {
    aoi <- compute_frequency_grid(
        files = x[["filepath"]],
        vert_min_height = vert_min_height,
        vert_max_height = vert_max_height
    )
}

# Do the thing.
aoi_ls <- lapply(
    files_df_ls,
    aoi_fn,
    vert_min_height = 250,
    vert_max_height = 1300
)


# Save aois to disc.
for (name in names(aoi_ls)) {
    filename <- 
    file.path(
        out_dir,
        paste0("aoi_", gsub(pattern = "[.]", replacement = "_", name), ".tif")
        #datatype = "INT4U
        #NAFlag=
    )
    terra::writeRaster(aoi_ls[[name]], filename = filename)
}



