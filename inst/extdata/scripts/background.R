####################################################################
# BACKGROUND
#-------------------------------------------------------------------
####################################################################


####################################################################
# CMQA DATA FLOW
# This script streamlines the data flow of the CQMA LAB AT INPE
####################################################################
# NOTES:
# a vertical profile is made of jars (12 or 17). Each jar is taken at certain height . A vertical profile corresponds to one flight
# in the lab, each jar is analyzed and gas concentration is observed
# for each jar (height) in the profile, a hysplit trajectory is computed
# each trajectory reach the sea at some point. We 're intereted ONLY in the first point
# for each trajectory' point-over-the-sea, we interpolate a gas concentration
#-------------------------------------------------------------------
# TODO:
# - run alf co and co2 at the same time. co2 produces no output. The control of the cycle is not only site but site & gas
# - add title to figures including gas name
# - save summary figures
####################################################################
# INSTALL PACKAGE
####################################################################
#require(devtools)
#devtools::install_github("lageeinpe/cqmaTools")
#source("/home/lagee/Documents/ghProjects/cqmaTools/R/util.R") # # source("/Users/lucas/Documents/ghProjects/cqmaTools/R/util.R") # 
####################################################################
# LOAD PACKAGES
####################################################################
# sudo apt-get install gdal-bin libgdal1-dev libproj-dev
# install.packages(c("roxygen2", "log4r", "fpc", "ggplot2"))
# install.packages(c("rgdal", "rgeos", "sp", "maps"))
suppressMessages(require("log4r"))
suppressMessages(require(cqmaTools))
suppressMessages(require(parallel))
suppressMessages(require(ggplot2))
suppressMessages(require(utils))
suppressMessages(require(fpc))
suppressMessages(require(rgdal))
suppressMessages(require(rgeos))
suppressMessages(require(sp))
suppressMessages(require(maps))

setwd("/home/lagee/Documents/ghProjects/cqmaTools")

suppressMessages(library(devtools))
devtools::load_all()

####################################################################
# WARNING: USE UNIX-LIKE OS
####################################################################
if (get_os() == "windows") {
  warning("Processing takes longer in windows because it is unable to use the package parallel. See ?mclapply")
}
####################################################################
# GET READY
####################################################################
test.path <- "/home/lagee/Documents/alber/test"

#-------------------------------------
# BRIEFCASES
#-------------------------------------
# magicc.path <- "/home/lagee/home 2/magicc/" # flight logs
#-------------------------------------
# STATIONS
#-------------------------------------
stationfile <- c(
  file.path(test.path, "stations", "rpbdaily"), 
  file.path(test.path, "stations", "ascdaily"), 
  file.path(test.path, "stations", "cptdaily")
)
limit.shp <- file.path(test.path, "shp", "limite.shp")
samerica.shp <- file.path(test.path, "shp", "continentalSouthAmericaLines.shp")
plot.path <- file.path(test.path, "plots")

# path to the resulting numeric files
data.out.path <- "/home/lagee/Documents/alber/test/BKG_results"

# still requires the gas attached to the path
hysplit.sim.path <- file.path(test.path, "hysplitsimulations") 

rawdatafile.vec <- list.files(file.path(test.path, "rawdata"), 
                              full.names = TRUE, recursive = FALSE, 
                              include.dirs = FALSE)
#-------------------------------------
# LOG
#-------------------------------------
logger <- create.logger()
logfile(logger) <- file.path(test.path, "background.log")
level(logger) <- "DEBUG"
info(logger, "Start! ###############################################")

####################################################################
# CONFIGURATION PARAMETERS
####################################################################

#-------------------------------------
# GENERAL
#-------------------------------------
# time zone used for data's dates and also for date computations
timezone <- "GMT"

# date tolerance in seconds. A tolerance used when comparing dates
tolerance.sec <- 10

#-------------------------------------
# RAW DATA PARAMETERS
#-------------------------------------

# name of a column name to filter raw data
flagcolname <- "flag"

# flags to keep in the raw data
keepFlags <- c("...", "..>", "..<")

#-------------------------------------
# INTERPOLATION PARAMETER
#-------------------------------------
# shapefile used to intersect the trajectories
limit.sp <- readOGR(dsn = dirname(limit.shp), 
                    layer = strsplit(basename(limit.shp), 
                                     split = '[.]')[[1]][[1]], 
                    verbose = FALSE) 

samerica.sp <- readOGR(dsn = dirname(samerica.shp), 
                       layer = strsplit(basename(samerica.shp), 
                                        split = '[.]')[[1]][[1]], 
                       verbose = FALSE)

# time offset for the trajectory (once over the sea) records to match station's
# data. i.e 2 days are (2 * 24 * 3600) * (-1) seconds
searchTranslation <- (2 * 24 * 3600) * (-1)

# number of +/- standard deviations used to filter the interpolated data into 
# backgorund
nsd <- 2

# maximum number of units away from the central tendency
maxfm.ppm <- 1.5

# TYPES of background computation:
# - Hard is applying twice the soft. 
# - Soft removes using a number standard deviations (nsd) to identify outliers 
#   and replace them by a trend measure i.e themedian
# - Median replace all the values by the median afther filtering outliers 
# - Cluster splits the data and applies soft in each cluster

#-------------------------------------
# TRAJECTORY PARAMETERS
#-------------------------------------

# Time to modify the hysplit file search. 10 days into the past is (10 * 24 * 3600) * (-1)
backTrajTime <- (10 * 24 * 3600) * (-1)

# keep trajectories above this height treshold
keepAbove <- 0

# Number of lines to remove from the header of hysplit's simulation files
simHeaderLines <- 7

# Filter trajectories which intersect west of this
inbound.minx = -70
inbound.maxx = NA
inbound.miny = NA
inbound.maxy = NA

# Are filtered trajectories included in plots?
plotAllTrajectories <- FALSE

# column names of hysplit files
HYSPLIT.COLNAMES <- c("V1", "V2", "year", "month", "day", "hour", "min", 
                      "V8", "V9", "lat", "lon", "height", "pressure") 

#-------------------------------------
# METEREOLOGICAL STATION DATA
#-------------------------------------
name <- c("RPB", "ASC", "CPT")
lon <- c(-59.430, -14.400, 18.189)
lat <- c(13.162, -7.967, -34.352)
stations.df <- data.frame(name, lon, lat, stationfile)
rm(name, lon, lat, stationfile)
#-------------------------------------
# MODELS OF TEMPERATURE AND PRESSURE
# h is height
#-------------------------------------
# ALFpres <- function(h){return(4E-6 *  h^2 - 0.1106 * h + 1004.5)}
# ALFtemp <- function(h){return(4E-7 *  h^2 - 0.0074 * h + 32.639)}
# RBApres <- function(h){return(4E-6 *  h^2 - 0.1209 * h + 1041.5)}
# RBAtemp <- function(h){return(2E-11 * h^2 - 0.006 * h + 29.996)}
# SANtemp <- function(h){return(3E-7 *  h^2 - 0.0067 * h + 29.92)}
# SANpres <- function(h){return(0    *  h^2 - 0.1054 * h + 1093.7)}
# TABtemp <- function(h){return(2E-7 *  h^2 - 0.0063 * h + 28.507)}
# TABpres <- function(h){return(4E-6 *  h^2 - 0.1121 * h + 1006.6)}
#-------------------------------------
# SITE HEIGHTS (mts)
#-------------------------------------
# siteheights <- list()
# siteheights["ALF"] <- 250
# siteheights["RBA"] <- 153
# # siteheights["SAN"] <- 152
# siteheights["TAB"] <- 188
# siteheights["TEF"] <- 188
#-------------------------------------
# PLOT
#-------------------------------------
plot2file <- TRUE                           # plots stored as files. Use NA for not plotting
goldrat <- (1 + sqrt(5))/2                  # width - height proportion 
device <- "png"                             # image format for data plots
map.xlim <- c(-80, 0)                       # map's min & max longitude
map.ylim <- c(-45, 35)                      # map's min & max latitude
map.height <- 8                             # map image size
map.width <- map.height * goldrat           # map image size
sec.width <- map.width                      # crosssection map image size
sec.height <- sec.width / goldrat           # crosssection map image size
prof.height <- map.height                   # profile image size
prof.width <- prof.height / goldrat         # profile image size

# spatial reference system assumed for geographic data 
SPATIAL.REFERENCE.SYSTEM <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# @param cnames     A character vector. The name of the columns of the raw data file
RAW.DATA.COLNAMES <- c("site", "year", "month", "day", "hour", "min", "flask", 
                       "V8", "concentration", "flag", "V11", "ayear", "amonth", 
                       "aday", "ahour", "amin", "lat", "lon", "height", 
                       "eventnumber", "flat", "flon", "fheight")

# The column names of the raw data file to keep after filtering
RAW.DATA.COLNAMES.KEEP <- c("site", "lat", "lon", "height", "year", "month", 
                            "day", "hour", "min", "flask", "concentration", 
                            "eventnumber")
# column anmes that make a profile
PROFILE.COLNAMES <- c("site", "year", "month", "day")
# metadata included in the trajectory's file names
TRAJ.FILENAMES.METADATA <- c("site", "year", "month", "day", "hour", "height")
####################################################################
# SCRIPT
####################################################################
flux.total.list <- list()
for (i in 1:length(rawdatafile.vec)) {
  rawdatafile.path <- rawdatafile.vec[i]
  info(logger, paste("Processing raw data:", rawdatafile.path, sep = " "))
  site <- unlist(strsplit(basename(rawdatafile.path), split = ".", fixed = TRUE))[1]
  gas <- unlist(strsplit(basename(rawdatafile.path), split = ".", fixed = TRUE))[2]
  base.path <-  file.path(test.path, "tmp", site, gas, fsep = .Platform$file.sep)
  #-----------------------------------------------------------------------------
  debug(logger, "step 00 - Check directories")
  #-----------------------------------------------------------------------------
  rawDataClean.path <- file.path(base.path, "rawDataFlag", fsep = .Platform$file.sep)
  hysplit.nohead.path <- file.path(base.path, "simNoHead", fsep = .Platform$file.sep)
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
