library(sf)
library(terra)
library(maps)
library(cqma)

source("influence_area_util.R")

hysplit_path <- "/home/alber/Documents/inpe/lagee/all_co2"

# build a matrix of sites and years
siteyear_mt <- do.call(rbind, unique(lapply(list.files(hysplit_path),
                                            function(x){
  spl <- strsplit(x, split = "_")[[1]]
  return(c("site" = spl[1], "year" = spl[2]))
})))

hotspots <- list()

for(i in seq(nrow(siteyear_mt))) {

  # Select a site and a year.
  plot_title <- paste(siteyear_mt[i, ], collapse = " ")

  # Filter files by site and year.
  files <- list.files(
    path = hysplit_path,
    pattern = paste(siteyear_mt[i,], collapse = "_"),
    full.names = TRUE,
    recursive = TRUE
  )

  # Process the files.
  hotspots[[paste(siteyear_mt[i,], collapse = "-")]] <-
    compute_influence_area(files, hs_skip = 7, hs_cnames = HYSPLIT.COLNAMES,
                           crs = 4326, min_lon = -80, max_lon = -30,
                           min_lat = -40, max_lat = 10, grid_resolution = 2,
                           plot_aoi = TRUE, plot_title = plot_title)
}

