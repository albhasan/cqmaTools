library(cqmaTools)



#---- Configuration ----
hysplit_path <- "/home/alber/Documents/github/cqmaTools/inst/extdata/trajectories/2011"
stopifnot("Hysplit data not found!" = dir.exists(hysplit_path))

out_dir <- "/home/alber/Downloads/tmp"
stopifnot("Output directory not found!" = dir.exists(out_dir))

HYSPLIT.COLNAMES <- c("V1", "V2", "year", "month", "day", "hour", "min", "V8", 
                      "V9", "lat", "lon", "height", "pressure")

#---- Load code ----

# Install/load packages.
required_packages <- c("sf", "terra", "maps")
if(any(!(required_packages %in% installed.packages()))) {
    install.packages(required_packages[!required_packages %in%
        installed.packages()],
        dependencies = TRUE)}
sapply(required_packages, require, character.only = TRUE)
rm(required_packages)



#---- Utility funcitons ----

#' Get the sample sites and years from data
#' 
#' @description
#' Get the sites' names and years from the file names in the given directory.
#'
#' @param hysplit_path a character(1). Path to a directory with hysplit files.
#'
#' @return a matrix with two columns: site and year.
#'
get_sites_years <- function(hysplit_path) {
    get_site_year_from_filename <- function(x) {
        spl <- strsplit(x, split = "_")[[1]]
        return(c("site" = spl[1], "year" = spl[2]))
    }
    return(do.call(
        rbind, 
        unique(lapply(list.files(hysplit_path),
            get_site_year_from_filename))
    ))
}



#---- Process data ----


# build a matrix of sites and years
siteyear_mt <- get_sites_years(hysplit_path)


for(i in seq(nrow(siteyear_mt))) {

    # Filter files by site and year.
    files <- list.files(
        path = hysplit_path,
        pattern = paste(siteyear_mt[i,], collapse = "_"),
        full.names = TRUE,
        recursive = TRUE
    )

    # Get a raster with the frequency of trajectory vertices.
    freq_ras <- compute_frequency_grid(
        files = files,
        hs_skip = 7,
        hs_cnames = HYSPLIT.COLNAMES,
        hs_clon = "lon",
        hs_clat = "lat",
        crs = 4326,
        min_lon = -80,
        max_lon = -30,
        min_lat = -40,
        max_lat = 10,
        grid_resolution = 2,
        min_height = 100,
        max_height = 3500
    )

    # Save raster to disc.
    terra::writeRaster(
        freq_ras,
        filename = file.path(
            out_dir, 
            paste0(paste(siteyear_mt[1,], collapse = "_"), ".tif")
        )
    )

}

