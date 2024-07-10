###############################################################################
# BACKGROUND
# This script streamlines the data flow of the CQMA LAB AT INPE
#------------------------------------------------------------------------------
# CMQA DATA FLOW
# NOTES:
# - A vertical profile is made of jars (12 or 17). Each jar is a sample taken 
#   at certain height . 
# - A vertical profile corresponds to one flight.
# - In the lab, each jar is analyzed and gas concentration is measured 
# - Each jar corresponds to a height in a profile. 
# - A hysplit trajectory is computed for each jar.
# - Each trajectory reaches the sea at some point. 
# - We're intereted ONLY in the first point of each trajectory' that is over 
#   the sea. 
# - We use this point to interpolate a gas concentration.
#-------------------------------------------------------------------
# TODO:
# - run alf co and co2 at the same time. co2 produces no output. The control of 
#   the cycle is not only site but site & gas
# - add title to figures including gas name
# - save summary figures


require(cqmaTools)

require(log4r)
require(parallel)
require(ggplot2)
require(utils)
require(fpc)
require(sf)
require(maps)



#---- Configuration ----

station_dir <- "/home/alber/Documents/cqma/data/test/stations"
stopifnot("Station directory not found" = dir.exists(station_dir))

# TODO: Update to the actual limit!
limit_file <- 
    "/home/alber/Documents/cqma/data/test/shp/fake_limit.shp"
stopifnot("Limit vector not found!" = file.exists(limit_file))

# TODO: Update to the actual lines.
samerica_shp <- 
    "/home/alber/Documents/cqma/data/test/shp/continentalSouthAmericaLines.shp"
stopifnot("S. America vector not found!" = file.exists(limit_file))

plot_dir <- "/home/alber/Documents/cqma/data/test/plots"
stopifnot("Plot directory not found" = dir.exists(plot_dir))

# Path to the resulting numeric files
data_out_dir <- 
    "/home/alber/Documents/cqma/data/test/BKG_results"
stopifnot("Result directory not found" = dir.exists(data_out_dir))

# Directory with results from Hysplit runs.
hysplit_sim_dir <- 
    "/home/alber/Documents/cqma/data/test/hysplitsimulations"
stopifnot("Hysplit simulation directory not found" = 
    dir.exists(hysplit_sim_dir))

raw_data_dir <- "/home/alber/Documents/cqma/data/test/rawdata"
stopifnot("Raw data directory not found" = dir.exists(raw_data_dir))

tmp_dir <- "/home/alber/Documents/cqma/data/test/tmp"
    stopifnot(dir.exists(tmp_dir))

logger <- create.logger()
logfile(logger) <- "/home/alber/Documents/cqma/data/test/background.log"
level(logger) <- "DEBUG"
info(logger, "Start! ###############################################")


#---- Validation ----

if (get_os() == "windows") {
  warning(paste("Processing takes longer in windows because it is unable to", 
                 "use the package parallel. See ?mclapply"))
}



#---- Process stations ----

rawdatafile_vec <- list.files(raw_data_dir, full.names = TRUE, 
                              recursive = FALSE, include.dirs = FALSE)




#---- Configuration parameters ----

# Time zone used for data's dates and also for date computations
timezone <- "GMT"

# Date tolerance in seconds. A tolerance used when comparing dates
tolerance.sec <- 10

# Name of a column name to filter raw data
flagcolname <- "flag"

# Flags to keep in the raw data
keepFlags <- c("...", "..>", "..<")

# Shapefile used to intersect the trajectories
limit_sf <- sf::read_sf(dsn = limit_file)
samerica_sf <- sf::read_sf(dsn = samerica_shp)

# Time offset for the trajectory (once over the sea) records to match station's
# data. i.e 2 days are (2 * 24 * 3600) * (-1) seconds
search_translation <- (2 * 24 * 3600) * (-1)

# Number of +/- standard deviations used to filter the interpolated data into 
# backgorund
nsd <- 2

# Maximum number of units away from the central tendency
maxfm_ppm <- 1.5

# TYPES of background computation:
# - Hard is applying twice the soft. 
# - Soft removes using a number standard deviations (nsd) to identify outliers 
#   and replace them by a trend measure i.e themedian
# - Median replace all the values by the median afther filtering outliers 
# - Cluster splits the data and applies soft in each cluster


# Time to modify the hysplit file search. 10 days into the past is 
# (10 * 24 * 3600) * (-1)
back_traj_time <- (10 * 24 * 3600) * (-1)

# keep trajectories above this height treshold
keep_above <- 0

# Number of lines to remove from the header of hysplit's simulation files
hs_skip <- 7

# Filter trajectories which intersect west of this
inbound_minx = -70
inbound_maxx = NA
inbound_miny = NA
inbound_maxy = NA

# Are filtered trajectories included in plots?
plot_all_trajectories <- FALSE

# Metereological station data
s_files <- c(
  "rpb" = file.path(station_dir, "rpbdaily.co2.txt"), 
  "asc" = file.path(station_dir, "ascdaily.co2.txt"), 
  "cpt" = file.path(station_dir, "cptdaily.co2.txt")
)
station_df <- 
    data.frame(
        name = c("RPB", "ASC", "CPT"),
        lon  = c(-59.430, -14.400, 18.189),
        lat  = c(13.162, -7.967, -34.352),
        station_files = c(s_files["rpb"], s_files["asc"], s_files["cpt"])
    )
rm(s_files)



#---- Plot setup ----

plot2file <- TRUE                   # Store plots. Use NA for not plotting
goldrat <- (1 + sqrt(5))/2          # width - height proportion 
device <- "png"                     # image format for data plots
map_xlim <- c(-80, 0)               # map's min & max longitude
map_ylim <- c(-45, 35)              # map's min & max latitude
map_height <- 8                     # map image size
map_width <- map_height * goldrat   # map image size
sec_width <- map_width              # crosssection map image size
sec_height <- sec_width / goldrat   # crosssection map image size
prof_height <- map_height           # profile image size
prof_width <- prof_height / goldrat # profile image size

# Spatial reference system assumed for geographic data 
spatial_reference_system <- 4326

# The column names of the raw data file to keep after filtering
rawdata_keep_cols <- c("site", "lat", "lon", "height", "year", "month", 
                       "day", "hour", "min", "flask", "concentration", 
                       "eventnumber")
stopifnot("Missing columns from raw data" = 
    all(rawdata_keep_cols %in% rawdata_keep_cols))

# Column names that make a profile
profile_colnames <- c("site", "year", "month", "day")

# Metadata included in the trajectory's file names
traj_filenames_metadata <- c("site", "year", "month", "day", "hour", "height")



#---- script ----

flux.total.list <- list()
for (i in seq(rawdatafile_vec)) {
  rawdata_file <- rawdatafile_vec[i]
  info(logger, paste("Processing raw data:", rawdata_file, sep = " "))
  site <- unlist(strsplit(basename(rawdata_file), split = ".", fixed = TRUE))[1]
  gas <- unlist(strsplit(basename(rawdata_file), split = ".", fixed = TRUE))[2]
  site_gas_dir <-  file.path(tmp_dir, site, gas, fsep = .Platform$file.sep)
  #-----------------------------------------------------------------------------
  debug(logger, "step 00 - Check directories")
  #-----------------------------------------------------------------------------
  rawdata_clean_dir <- file.path(site_gas_dir, "rawDataFlag", 
                                 fsep = .Platform$file.sep)
  hysplit.nohead.path <- file.path(site_gas_dir, "simNoHead", 
                                   fsep = .Platform$file.sep)
  # create the missing folders
  folder.vec <- c(rawDataClean.path, hysplit.nohead.path)
  for (folder in folder.vec[!dir.exists(folder.vec)]) {
    dir.create(folder, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  }
  # delete the old files
  for (folder in folder.vec[dir.exists(folder.vec)]) {
    files.vec <- list.files(path = folder, full.names = TRUE)
    file.remove(files.vec)
  }
  #-----------------------------------------------------------------------------
  debug(logger, "step 01 - Drop columns and filter raw data")
  #-----------------------------------------------------------------------------
  filterRawfile <- splitRawdata(file.in = rawdatafile.path, 
                                path.out = rawDataClean.path, 
                                colname = flagcolname, 
                                keepFlags = keepFlags,
                                cnames = RAW.DATA.COLNAMES,
                                keepCols = RAW.DATA.COLNAMES,
                                cnamesTest = RAW.DATA.COLNAMES)
  wrongcoords <- filterRawfile[[3]]
  duplicatedRows <- filterRawfile[[2]]
  filterRawfile <- filterRawfile[[1]]
  if (nrow(wrongcoords)  > 0) {
    logger::log_warn(paste("Wrong coords found in raw data file:", 
                           nrow(wrongcoords), "\n", df2text(wrongcoords), 
                           sep  = " "))
  }
  if (nrow(duplicatedRows)  > 0) {
    logger::log_warn(paste("Duplicated or inconsistent  rows in raw data file:", 
                           nrow(duplicatedRows), "\n", df2text(duplicatedRows), 
                           sep  = " "))
  }
  #-----------------------------------------------------------------------------
  debug(logger, "step 02 - Check Hysplit files")
  #-----------------------------------------------------------------------------
  # add the gas name to path of the hysplit files
  hysplit.gas.path <- file.path(hysplit.sim.path, gas, fsep = .Platform$file.sep)
  if (length(list.files(hysplit.gas.path)) == 0) {
    logger::log_error(paste("Unable to continue: No hysplit and no trajectory files in", hysplit.gas.path, sep = " "))
    stop()
  }
  #-----------------------------------------------------------------------------
  debug(logger, "step 03 - Remove header from HYSPLIT files")
  #-----------------------------------------------------------------------------
  file.vec <- list.files(path = hysplit.gas.path, 
                         full.names = TRUE, 
                         pattern = paste(toupper(site), "_*", sep = ""), 
                         ignore.case = TRUE)
  if (length(file.vec) == 0) {
    warn(logger, paste("No trajectories found for", rawdatafile.path, sep = " "))
    break
  }
  
  hysplit.nohead.files <- tryCatch({
    removeHeaders(file.vec = file.vec, 
                  path.out = hysplit.nohead.path, 
                  skip = simHeaderLines, 
                  cnames = HYSPLIT.COLNAMES)    
  }, error = function(e) {
    logger::log_error("Error in step 3. Something is wrong with the HYSPLIT files")
  })
  hysplit.nohead.files <- data.frame(as.vector(unlist(hysplit.nohead.files)), 
                                     rep(TRUE, times = length(hysplit.nohead.files)), 
                                     stringsAsFactors = FALSE)
  colnames(hysplit.nohead.files) <- c("file.vec", "keep")
  #-----------------------------------------------------------------------------
  debug(logger, "step 04 - Check for trajectories that hit the gound")
  #-----------------------------------------------------------------------------
  # run the filter
  hysplit.traj.files <- filterTrajHeight(file.vec = as.vector(unlist(
    hysplit.nohead.files[hysplit.nohead.files$keep == TRUE, "file.vec"]
  )), 
  above = keepAbove, cnames = HYSPLIT.COLNAMES)
  # merge results
  colnames(hysplit.traj.files) <- c("file.vec", "above" )
  hysplit.nohead.files <- merge(hysplit.nohead.files, hysplit.traj.files, 
                                by = "file.vec", all = TRUE)
  # report
  if (sum(!hysplit.traj.files["above"]) > 0) {
    warn(logger,
         paste("Some trajectories hit the gound: ", 
               sum(!hysplit.traj.files["above"]),  sep = "")
    )
  }
  # update
  hysplit.nohead.files["keep"] <- hysplit.nohead.files["keep"] & hysplit.nohead.files["above"]
  hysplit.nohead.files[is.na(hysplit.nohead.files["keep"]), "keep"] <- FALSE
  #-----------------------------------------------------------------------------
  debug(logger, "step 05 - Intersect trajectories with limit.shp")
  #-----------------------------------------------------------------------------
  traj.intersections <- intersectTraj(file.vec = as.vector(unlist(
    hysplit.nohead.files[hysplit.nohead.files$keep == TRUE, "file.vec"]
  )), 
  limit.in = limit.sp, cnames = HYSPLIT.COLNAMES, srs = SPATIAL.REFERENCE.SYSTEM)
  #-----------------------------------------------------------------------------
  log4r::debug(logger, "step 06 - Filter trajectories which do not reach the sea")
  #-----------------------------------------------------------------------------
  # run the filter
  traj.2thesea <- trajreachthesea(traj.intersections = traj.intersections)
  # merge results
  colnames(traj.2thesea) <- c("file.vec", "sea" )
  hysplit.nohead.files <- merge(hysplit.nohead.files, traj.2thesea, 
                                by = "file.vec", all = TRUE)
  # report
  traj.inland <- logical(length = length(traj.intersections[[1]]))
  if (nrow(traj.2thesea) > 0) {
    traj.inland <- !traj.2thesea["sea"]
  }
  if (sum(traj.inland) > 0 | length(traj.inland) == 0) {
    log4r::warn(logger, paste("Some trajectories don't reach the sea: ", 
                              sum(traj.inland), sep = ""))
  }
  # update
  hysplit.nohead.files["keep"] <- hysplit.nohead.files["keep"] & hysplit.nohead.files["sea"]
  hysplit.nohead.files[is.na(hysplit.nohead.files["keep"]), "keep"] <- FALSE
  #-----------------------------------------------------------------------------
  debug(logger, "step 07 - Filter trajectories falling out of bounds")
  #-----------------------------------------------------------------------------
  # run the filter
  traj.inbound <- trajinbound(traj.intersections = traj.intersections, 
                              minx = inbound.minx, maxx = inbound.maxx, 
                              miny = inbound.miny, maxy = inbound.maxy)
  # merge results
  colnames(traj.inbound) <- c("file.vec", "inBound" )
  hysplit.nohead.files <- merge(hysplit.nohead.files, traj.inbound, 
                                by = "file.vec", all = TRUE)
  # report
  traj.out <- logical(length = length(traj.intersections[[1]]))
  if (nrow(traj.inbound) > 0) {
    traj.out <- !traj.inbound["inBound"]
  }
  if (sum(traj.out, na.rm = TRUE) > 0 | length(traj.out) == 0) {
    traj.out[is.na(traj.out)] <- FALSE
    warn(logger, paste("Some trajectories are out of bounds: ", sum(traj.out), sep = ""))
    #debug(logger, paste("Some trajectories are out of bounds: ", sum(traj.out), " \n", paste(traj.inbound[traj.out, 1], collapse = " \n"), sep = ""))
  }
  # update
  hysplit.nohead.files["keep"] <- hysplit.nohead.files["keep"] & hysplit.nohead.files["inBound"]
  hysplit.nohead.files[is.na(hysplit.nohead.files["keep"]), "keep"] <- FALSE
  #-----------------------------------------------------------------------------
  debug(logger, "step 08 - Filter trajectories falling out of the stations' range")
  #-----------------------------------------------------------------------------
  # run the filter
  stations.df$stationfile <- paste(stations.df$stationfile, gas, "txt", sep = ".")
  traj.inStation <- trajOutInterpolation(traj.intersections = traj.intersections, 
                                         stations.df = stations.df)
  # merge results
  colnames(traj.inStation) <- c("file.vec", "inStation" )
  hysplit.nohead.files <- merge(hysplit.nohead.files, traj.inStation, 
                                by = "file.vec", all = TRUE)
  # report
  traj.out <- logical(length = length(traj.intersections[[1]]))
  if (nrow(traj.inStation) > 0) {
    traj.out <- !traj.inStation["inStation"]
  }
  if (sum(traj.out, na.rm = TRUE) > 0 | length(traj.out)  == 0) {
    warn(logger, paste("Some trajectories are out of reach of stations: ", 
                       sum(traj.out), sep = ""))
    #debug(logger, paste("Some trajectories are out of reach of stations: ", sum(traj.out), " \n", paste(traj.inStation[traj.out, 1], collapse = " \n"), sep = ""))
  }
  # update
  hysplit.nohead.files["keep"] <- hysplit.nohead.files["keep"] & hysplit.nohead.files["inStation"]
  hysplit.nohead.files[is.na(hysplit.nohead.files["keep"]), "keep"] <- FALSE
  #-----------------------------------------------------------------------------
  log4r::debug(logger, "step 09 - Interpolate data for the trajectory's over-the-sea point to the metereological stations")
  #-----------------------------------------------------------------------------
  keepAaboveAseaAstation <- as.vector(unlist(hysplit.nohead.files["keep"]))
  traj.intersections[[1]] <- traj.intersections[[1]][keepAaboveAseaAstation]
  traj.intersections[[2]] <- traj.intersections[[2]][keepAaboveAseaAstation]
  traj.interpolations <- crossdata(
    traj.intersections = traj.intersections, 
    stations.df = stations.df, 
    tolerance.sec = tolerance.sec, 
    timezone = timezone, 
    searchTranslation = searchTranslation
  )
  stopifnot(length(traj.interpolations) == length(traj.intersections[[1]]))
  stopifnot(length(traj.interpolations) == length(traj.intersections[[2]]))
  #-----------------------------------------------------------------------------
  # flush the filter summary of the trajectories
  #-----------------------------------------------------------------------------
  t <- paste("\n", paste(colnames(hysplit.nohead.files), collapse = " "), sep = "")
  for (nr in 1:nrow(hysplit.nohead.files)) {
    t <- paste(t, paste(hysplit.nohead.files[nr, ], collapse = " "), sep = "\n")
  }
  info(logger, t)
  #-----------------------------------------------------------------------------
  debug(logger, "step 10 - Plot & save results")
  #-----------------------------------------------------------------------------
  traj.plot <- NA
  if (plotAllTrajectories) {
    traj.plot <- hysplit.nohead.files[hysplit.nohead.files$keep == FALSE, "file.vec"]
  }
  plot.result <- plotTrajbackground(
    file.in = filterRawfile, 
    path.out = plot.path, 
    traj.plot = traj.plot, 
    traj.interpol = traj.interpolations, 
    traj.intersections = traj.intersections, 
    #use.backgorund = use.backgorund, 
    device = device, 
    map.xlim = map.xlim, 
    map.ylim = map.ylim, 
    map.height = map.height, 
    map.width = map.width, 
    sec.width = sec.width, 
    sec.height = sec.height, 
    prof.height = prof.height, 
    prof.width  = prof.width, 
    nsd = nsd, 
    maxfm.ppm = maxfm.ppm, 
    stations.df = stations.df, 
    plot2file = plot2file,
    logger = logger, 
    trajCnames = HYSPLIT.COLNAMES, 
    obsCnames = RAW.DATA.COLNAMES, 
    profileCnames = PROFILE.COLNAMES, 
    trajFileMet = TRAJ.FILENAMES.METADATA
  )
  plot.files <- plot.result[[1]]
  profile.df <- plot.result[[2]]
  #-----------------------------------------------------------------------------
  debug(logger, "step 11 - plot trajectories by year")
  #-----------------------------------------------------------------------------
  if (nrow(hysplit.nohead.files) > 0) {
    siteyearplot.list <- plotTrajYear(file.vec = as.vector(unlist(hysplit.nohead.files["file.vec"])), 
                                      path.out = plot.path, 
                                      device = device, 
                                      map.xlim = map.xlim, 
                                      map.ylim = map.ylim, 
                                      map.height = map.height, 
                                      map.width = map.width, 
                                      stations.df = stations.df, 
                                      plot2file = plot2file)
  }
  #-----------------------------------------------------------------------------
  debug(logger, "step 12 - time to the sea")
  #-----------------------------------------------------------------------------
  traj.intersectionsSA <- intersectTraj(
    file.vec = as.vector(
      unlist(
        hysplit.nohead.files[, "file.vec"]
      )
    ), 
    limit.in = samerica.sp,
    cnames = HYSPLIT.COLNAMES,
    srs = SPATIAL.REFERENCE.SYSTEM
  )
  
  intersect_rows <- unlist(
    lapply(traj.intersectionsSA[[2]], function(x){
      res <- NA
      if(!is.null(x)){
        res <- as.integer(rownames(x))
      }
      return(res)
    })
  )
  trajtime <- computeTrajTime(file.vec = traj.intersectionsSA[[1]], 
                              line.vec = intersect_rows, 
                              cnames = HYSPLIT.COLNAMES) # total time of the trajectory over the main land
  
  trajtime.df <- data.frame(basename(traj.intersectionsSA[[1]]), as.double(trajtime), stringsAsFactors = FALSE)
  colnames(trajtime.df) <- c("file.vec", "trajtime.days")
  profile.df <- merge(profile.df, trajtime.df, by = "file.vec", all = TRUE)
  profile.list <- lapply(split(profile.df, f = as.factor(profile.df$profile)), function(x){
    amean <- mean(as.double(x$trajtime), na.rm = TRUE)
    x$trajtime[is.na(x$trajtime)] <- amean
    return(x)
  })
  profile.df <- do.call(rbind, profile.list)
  #-----------------------------------------------------------------------------
  debug(logger, "step 13 - Write result table")
  #-----------------------------------------------------------------------------
  write.table(profile.df, file = file.path(data.out.path, paste(basename(rawdatafile.vec), "_bkgTable.txt", sep = ""), fsep = .Platform$file.sep))
}

#-----------------------------------------------------------------------------
options(nwarnings = 100000)  
warnings()
debug(logger, "END OF SCRIPT")
#-----------------------------------------------------------------------------
