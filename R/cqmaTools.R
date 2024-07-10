#---- NOTES: ----
# a vertical profile is made of jars (12 or 17). Each jar is taken at certain height . A vertical profile corresponds to one flight
# in the lab, each jar is analyzed and gas concentration is observed
# for each jar (height) in the profile, a hysplit trajectory is computed
# each trajectory reach the sea at some point. We 're intereted ONLY in the first point
# for each trajectory' point-over-the-sea, we interpolate a gas concentration

#---- TODO: ----

#---- BACKGROUND ----


#' @title Filter trajectories by height
#' @name filterTrajHeight
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Check if the trajectories are above the given treshold
#'
#' @param file.vec A character vector. The paths to the input files
#' @param above    A length-1 numeric. The treshold
#' @param cnames   A character vector. The column names of the hysplit files
#' @return         A data frame with one row for each file and 2 columns: The trajectories' path and a boolean indicating if they meet the test
#' @export
filterTrajHeight <- function(file.vec, above, cnames){
  # check trajectories' height and make a vector of those to keep
  #cnames <- HYSPLIT.COLNAMES                                                    # column names of the input file    
  file.dat.list <- files2df(files = file.vec, header = FALSE, 
                            skip = 0, cnames = cnames)
  keep <- vector(mode = "logical", length = length(file.dat.list))
  keep <- lapply(file.dat.list, function(x){if(sum(x$height < above) > 0){return(FALSE)}; return(TRUE)}) # test
  keep <- as.vector(unlist(keep))
  return(data.frame(file.vec, keep))
}



#' @title Split a raw data file.
#' @name splitRawdata
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Split a single raw data file. The output files are prefixed with 
#' "traj_specs_".
#'
#' @param file.in    A character.Path to a raw data file
#' @param path.out   A character. Path to the folder for storing the resulting files
#' @param colname    A character. The name of a column where the flags are located
#' @param keepFlags  A character vector. Flags to keep in the raw data. i.e. c("...", "..>")
#' @param cnames     A character vector. The name of the columns of the raw data file
#' @param keepCols   A character vector. The column names of the raw data file to keep after filtering
#' @param cnamesTest A character vector. The column names used for testing duplicated rows in the filtered data
#' @return           A list of three. A character with the name of the new file, a data.frame of duplicated or inconsistent rows, and a data.frame of coordinates which were changed because they have the wrong sign
#' @export
splitRawdata <- function(file.in, path.out, colname, keepFlags, cnames, 
                         keepCols, cnamesTest){
  # raw data column names
  #cnames <- RAW.DATA.COLNAMES
  #keepCols <- RAW.DATA.COLNAMES.KEEP                                            # keep these columns
  #cnamesTest <- RAW.DATA.COLNAMES.TESTDUPLICATED
  
  file.dat <- file2df(file.in = file.in, header = FALSE, skip = 0,             # read data
                       cnames = cnames)
  # filter data
  file.dat <- .filterDataframe(df = file.dat, keepCols = keepCols,              # filter using flag attribute
                               flagName = colname, keepFlags = keepFlags)
  # report duplicated rows
  testUnique.vec <- apply(file.dat[, cnamesTest], 
                          MARGIN = 1, 
                          function(x){
                            gsub(" ", "0", paste(unlist(x), collapse = "___"))
                          })
  testUnique.df <- as.data.frame(table(as.vector(testUnique.vec)), 
                                 stringsAsFactors = FALSE)
  dup.df <- testUnique.df[testUnique.df$Freq > 1, ]
  # test lat lon for missing signs
  file.dat.list <- split(file.dat, file.dat$site)
  nsd = 3                                                                       # number of standard deviationto identify an outlier 
  treshold = 10                                                                 # the minimum difference (in SDs) to accept a sign  change in coordinates
  #
  outll <- parallel::mclapply(file.dat.list, 
                              .checklonlat, 
                              nsd = nsd, 
                              treshold = treshold)
  #
  # replace coords
  file.dat <- do.call("rbind", file.dat.list)
  ll.df <- do.call("rbind", outll)
  file.dat[, c("lon", "lat")] <- ll.df[, c("lon", "lat")]
  # store the results
  newfile <- file.path(path.out, paste("traj_specs_", 
                                       basename(file.in), sep = ""), 
                       fsep = .Platform$file.sep)
  utils::write.table(file.dat, file = newfile, col.names = FALSE, 
                     row.names = FALSE, quote = FALSE)
  # report  
  wrongcoords <- ll.df[ll.df$changed == TRUE, c("lon", "lat")] * (-1)
  wrongcoords["idrow"] <- rownames(wrongcoords)
  colnames(dup.df) <- c("shYMDhmflask", "Freq")
  dup.df["idrow"] <- as.numeric(rownames(dup.df))
  return(list(newfile, dup.df, wrongcoords))
}



#' @title Check if the trajectories reach beyond the limit (the sea)
#' @name trajreachthesea
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Check if the trajectories reach beyond the limit (the sea)
#'
#' @param traj.intersections A list made of a character vector and a list. The character vector is the path to each trajectory file while the list contains the first row in the trajectory file which lies over the sea
#' @return                   A data.frame with one row for each file and 2 columns: The trajectories' path and a boolean indicating if they were kept
#' @export
trajreachthesea <- function(traj.intersections){
  file.vec <- unlist(traj.intersections[[1]])
  trajintersect.list <- traj.intersections[[2]]
  keep <- logical()
  if(length(file.vec) == 0){warning("No input files!"); return(data.frame(file.vec, keep))}
  # get the files with at least one row, that is, the trajectories which reach to the sea
  keep <- unlist(lapply(trajintersect.list, is.null))
  keep <- !keep
  return(data.frame(file.vec, keep))
}




#' @title Are trajectories inside boundaries?
#' @name trajinbound
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Check if the trajectory intersection is in the given boundary
#'
#' @param traj.intersections A list of two obejcts. A character vector and a list of data.frames. The data.frames are made of the rows in the matching trajectory file which first crossed to the sea
#' @param minx               A numeric. The minimum X
#' @param maxx               A numeric. The maximum Y
#' @param miny               A numeric. The minimum Y
#' @param maxy               A numeric. The maximum X
#' @return                   A data frame
#' @export
trajinbound <- function(traj.intersections, minx, maxx, miny, maxy){
  keep <- logical()
  file.vec <- unlist(traj.intersections[[1]])
  trajintersect.list <- traj.intersections[[2]]
  if(length(file.vec) == 0){warning("No input files!"); return(data.frame(file.vec, keep))}
  keeptraj <- sapply(trajintersect.list, is.null)
  keeptraj <- !keeptraj
  kfile.vec <- file.vec[keeptraj]
  ktrajintersect.list <- trajintersect.list[keeptraj]
  ktrajintersect.df <- do.call("rbind", ktrajintersect.list)
  xy.df <- ktrajintersect.df[, c("lon", "lat")]
  colnames(xy.df) <- c("x", "y")
  keep <- .inbound(xy.df = xy.df, minx = minx, maxx = maxx, 
                   miny = miny, maxy = maxy)
  k <- as.data.frame(cbind(kfile.vec, keep), stringsAsFactors = FALSE)
  names(k) <- c("file.vec", "keep" )
  file.vec <- as.data.frame(file.vec, stringsAsFactors = FALSE)
  colnames(file.vec) <- "file.vec"
  file.vec <- merge(x = file.vec, y = k, by = "file.vec", all.x = TRUE)
  file.vec$keep <- as.logical(file.vec$keep)
  return(file.vec)
}




# 
#
#' @title Are trajectories inside the metereological station coverage?
#' @name trajOutInterpolation
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Check if the trajectories' intersections fall inside the metereological station coverage
#'
#' @param traj.intersections A list made of a character vector and a list. The character vector is the path to each trajectory file while the list contains the first row in the trajectory file which lies over the sea
#' @param stations.df        A data.frame of metereological station data. It must have at least 4 columns: name, longitude, latitude, and file c("name", "lon", "lat", "file")
#' @return                   A data.frame with one row for each file and 2 columns: The trajectories' path and a boolean indicating if meet the test
#' @export
trajOutInterpolation <- function(traj.intersections, stations.df){
  stations.df <- stations.df[order(stations.df$lat),]                           # sort by latitude. This is mandatory
  invalid <- unlist(lapply(traj.intersections[[2]], is.null))                   # mark invalid
  file.vec <- unlist(traj.intersections[[1]])[!invalid]                         # The first list contains the paths to trajectory files
  trajrecords.df <- do.call("rbind", traj.intersections[[2]])                   # The second list contains the intersections. That is, the trajectories' first row over the sea. One row per trajectory
  matchInterval <- .inInterval(val = unlist(trajrecords.df["lat"]),             # which stations should be used for each trajectory interpolation? . This is the match of trajectories to stations for interpolation 
                               vec = unlist(stations.df["lat"]))
  keep <- matchInterval[,1] == TRUE | matchInterval[,2] == TRUE                 
  pres <- cbind(file.vec, as.vector(keep))                                      # who passes the test?
  # match to the original files
  ti1.df <- as.data.frame(traj.intersections[[1]])
  colnames(ti1.df) <- "file.vec"
  res <- merge(ti1.df, pres, by = "file.vec", all = TRUE)
  colnames(res) <- c("file.vec", "keep")
  res["keep"] <- as.logical(res$keep)                                           # R, you motherfucking piece of shit!     
  return(res)
}



#' @title Get the stations' data matching the trajectory records
#' @name crossdata
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function takes a trajectory's first point beyond the limit (over the sea) and project its latitude to an straight line made by the corresponding metereological stations
#'
#' @param traj.intersections A list made of a character vector and a list. The character vector is the path to each trajectory file while the list contains the first row in the trajectory file which lies over the sea
#' @param stations.df        A data.frame of metereological station data. It must have at least 4 columns: name, longitude, latitude, and file c("name", "lon", "lat", "file")
#' @param tolerance.sec      A numeric. A tolerance used when comparing dates
#' @param timezone           A character. The time zone. i.e. "GMT"
#' @param searchTranslation  A numeric. The number of seconds for searching metereological station data
#' @return                   A list of numeric. Each number is the interpolation result from the matching stations
#' @export
crossdata <- function(traj.intersections, stations.df, tolerance.sec, timezone, 
                      searchTranslation){
  interpolation.res <- list()
  # sort by latitude. This is mandatory
  stations.df <- stations.df[order(stations.df$lat),]                           
  if (length(traj.intersections[[1]]) == 0)
    return(interpolation.res)
  if (length(traj.intersections[[2]]) == 0)
    return(interpolation.res)
  
  # filter latitudes
  # joins all the data.frames from records into a single one. 
  # NOTE1: it assumes all the data.frames have the same columns, in the same order
  # NOTE2: it assumes a single row on each record. Otherwise the following calculations are WRONG
  
  # The first list contains the paths to trajectory files
  trajfiles <- unlist(traj.intersections[[1]])
  
  # The second list contains the intersections. That is, the trajectories' first row over the sea. One row per trajectory
  trajrecords.df <- do.call("rbind", add_na_df(traj.intersections[[2]]))
 
  stopifnot(nrow(trajrecords.df) == length(traj.intersections[[2]]))
   
  # which stations should be used for each trajectory interpolation?
  # This is the match of trajectories to stations for interpolation
  matchInterval <- .inInterval(val = unlist(trajrecords.df["lat"]),
                               vec = unlist(stations.df["lat"])) 
  out <- matchInterval[,1] == FALSE & matchInterval[,2] == FALSE
  if (sum(out, na.rm = TRUE) > 0)
    warning("Trajectories falling out of interpolation zone:", 
            sum(out, na.rm = TRUE), " \n", 
            paste(trajfiles[which(out)], collapse = " \n"))
  # read station's data from files
  station.dat.list <- list()
  for (i in 1:nrow(stations.df)) {
    station.dat <- utils::read.table(file = as.character(stations.df[i, "stationfile"]), sep = "", header = FALSE)
    colnames(station.dat) <- c("datedec", "V2")
    station.dat.list[[i]] <- station.dat
  }
  # add a column with normal dates instead of decimal year dates
  station.dat.list <- parallel::mclapply(station.dat.list, 
                                         function(x){
                                           x["date"] <- unlist(lapply(unlist(x["datedec"]), ydec2date)); 
                                           return(as.data.frame(x))
                                         }) # stations' data
  # do the interpolation
  interpolation.res <- lapply(
    1:nrow(trajrecords.df), 
    .crossdata.aux, 
    traj.records = trajrecords.df, 
    traj.data.match = matchInterval, 
    stations = stations.df, 
    stations.dat = station.dat.list, 
    # match station's data X days before the records' data
    searchTranslation = searchTranslation,
    timezone = timezone, 
    tolerance.sec = tolerance.sec
  )
  return(interpolation.res)
}




#' @title Plot profiles and trajectories
#'
#' @description Plot the input data into map, section, and profile graphs. These plots are stored in disk
#'
#' @param file.in            A character. The path to a filtered observations file or flag filtered raw data
#' @param path.out           A character. The path to a folder to store the results
#' @param traj.plot          A vector of character. The file path to trajectories to plot additional to those in traj.intersections[[1]]
#' @param traj.interpol      A list of numeric. The interpolated values of teh trajectories over the sea
#' @param traj.intersections A list made of a character vector and a list. The character vector is the path to each trajectory file while the list contains the first row in the trajectory file which lies over the sea
#' @param device             A character. Image format, i.e. PNG
#' @param map.xlim           A numeric vector. Map's min & max longitude
#' @param map.ylim           A numeric vector. Map's min & max latitude
#' @param map.height         A numeirc. Map image size
#' @param map.width          A numeirc. Map image size
#' @param sec.width          A numeric. Crosssection map image size
#' @param sec.height         A numeric. Crosssection map image size
#' @param prof.height        A numeric. Profile image size
#' @param prof.width         A numeric. Profile image size
#' @param nsd                A numeric. Number of standard deviations to use to filter the interpolated data
#' @param maxfm.ppm          A numeric. Maximum number of units away from the central tendency
#' @param stations.df        A data.frame with metereological station data. It must contain at least the columns c("name", "lon", "lat")
#' @param plot2file          A logical or NA. Should plots be stored as files? Use NA to not plot anything
#' @param trajCnames         A character. Column names of hysplit files
#' @param obsCnames          A character. Filtered observation column names
#' @param profileCnames      A character. Column names of the pofiles
#' @param trajFileMet        A character. Metadata included in the trajectory's file names
#' @return                   A list of objects. A character vector with the paths to the plot files, and a data.frame with merged data
#' @export
plotTrajbackground <- function(file.in, path.out, traj.interpol, 
                                traj.intersections, traj.plot, 
                                device, map.xlim, 
                                map.ylim, map.height, map.width, sec.width, 
                                sec.height, prof.height, prof.width, nsd, 
                                maxfm.ppm, stations.df, plot2file,
                               trajCnames, obsCnames, profileCnames, trajFileMet){
  ## @param use.backgorund     A character. The type of filter used when calculating the background concentration. The options are c("median", "hard")
  
  if (length(traj.interpol) == 0) {
    warning("No interpolations!")
    return()
  }
  #trajCnames <- HYSPLIT.COLNAMES                                                # column names of the trajectory files
  #obsCnames <- RAW.DATA.COLNAMES.KEEP                                           # filtered observation column names
  #profileCnames <- PROFILE.COLNAMES
  lon <- 0; lat <- 0; filename <- 0; height <- 0; concentration <- 0; type <- 0 # avoid notes during package check
  profil <- 0;file.vec <- 0;  sheight <- 0; profile <- 0; trajlabel <- 0
  #
  # observation data
  #
  raw.df <- file2df(file.in = file.in, header = FALSE,                         # flag filtered raw data
                     skip = 0, cnames = obsCnames) 
  raw.df["profile"] <- apply(raw.df[, profileCnames],                        # add profile column to raw data
                             MARGIN = 1, 
                             function(x){
                               x[[3]] <- formatC(x[[3]], width = 2, flag = "0") # 
                               x[[4]] <- formatC(x[[4]], width = 2, flag = "0")
                               return(gsub(" ", "0", paste(unlist(x), collapse = "_")))
                             })
  #
  # trajectory data
  #
  traj.file.vec <- c(traj.intersections[[1]], traj.plot)                        # all the trajectory file names 
  traj.file.vec <- traj.file.vec[!is.na(traj.file.vec)]                         # remove NAs
  traj.dat.list <- files2df(files = traj.file.vec,                          # read the trajectory files into a list of data.frames 
                             header = FALSE, skip = 0, cnames = trajCnames)
  traj.dat.list <- .listname2data.frame(df.list = traj.dat.list,                # add file name as column
                                        colname = "file.vec")
  traj.dat.df <- do.call("rbind", traj.dat.list)                                # collapse to a single data.frame
  traj.dat.df["profile"] <- filename2profile(unlist(traj.dat.df["file.vec"]))  # add profile column
  traj.dat.df["trajlabel"] <- format_traj_names(unlist(traj.dat.df["file.vec"]))  # add a label to order by height in the plot
  #
  # process interpolated data
  #
  interpol.df <- data.frame(traj.intersections[[1]], unlist(traj.interpol), 
                            stringsAsFactors = FALSE)
  names(interpol.df) <- c("file.vec", "interpolated")
  interpol.df["file.vec"] <- basename(as.vector(unlist(interpol.df["file.vec"])))
  interpol.df["profile"] <- filename2profile(unlist(interpol.df["file.vec"]))  # add profile column
  interpol.df <- cbind(interpol.df, 
                       .trajFilenames2metadata(
                         file.vec = unlist(interpol.df["file.vec"]), 
                         cnames = trajFileMet))
  interpol.df["height"] <- .as.numeric.factor(interpol.df$height)
  #
  # process intersections
  #
  
  intersec.df <- cbind(basename(traj.intersections[[1]]), 
                       do.call("rbind", add_na_df(traj.intersections[[2]])))
  
  intersec.df["filerow"] <- as.numeric(rownames(intersec.df))
  colnames(intersec.df) <- c("file.vec", "V1", "V2", "syear", "smonth",         # rename columns
                             "sday", "shour", "smin", "V8", "V9", 
                             "slat", "slon", "sheight", 
                             "spressure", "filerow")
  intersec.df["syear"] <- unlist(intersec.df["syear"]) + 2000
  intersec.df["profile"] <- filename2profile(as.vector(unlist(intersec.df["file.vec"])))  # add profile column
  intersec.df["trajlabel"] <- format_traj_names(as.vector(unlist(intersec.df["file.vec"])))
  #
  # base map
  #
  basemap <- .buildbasemap(stations.df, map.xlim, map.ylim)
  #
  # plot
  #
  profile.vec <- unique(unlist(interpol.df["profile"]))                         # each profile to process below
  filenames <- vector(mode = "character", length = 0)                           # files created on this function
  profile.all <- data.frame()                                                   # keep the profile data  
  for (prof in profile.vec) {
    #
    # merge data
    #
    
    #---- TODO: error ----
    # nrow(prof.obs) == 0 because the naming convention of profile changed????
    prof.obs <- raw.df[raw.df$profile == prof, ]                                # observed data
    if (nrow(prof.obs) < 1) {
      warning(" - - No observations found for ", prof, sep = "")
      next()
    }
    
    prof.int <- interpol.df[interpol.df$profile == prof, ]                      # interpolated data
    prof.isec <- intersec.df[intersec.df$profile == prof, ]                     # trajectory data of the intersections
    prof.traj <- traj.dat.df[traj.dat.df$profile == prof, ]                     # all the trajectories of this profile
    # complete interpolation to match the observations
    prof.obs <- merge(x = prof.obs, 
                      y = prof.int[, c("file.vec", "interpolated", "height")], 
                      by = "height", all.x = TRUE)
    colnames(prof.obs)[colnames(prof.obs) == "concentration"] <- "observed"
    #
    # background calculation
    #
    my_data_vec <- as.vector(unlist(prof.obs["interpolated"]))
    if (length(my_data_vec) < 2) {
      stop(sprintf("Profile %s doesn't have enough heights.", prof))
      #next()
    }
    back.df <- .background.softrules(data.vec = my_data_vec, 
                                     nsd = nsd, maxfm.ppm = maxfm.ppm)
    #
    # merge more data
    #
    prof.obs <- cbind(prof.obs, back.df)
    prof.obs <- merge(x = prof.obs, y = subset(prof.isec, select = -profile ), 
                      by = "file.vec", all.x = TRUE)
    profile.all <- rbind(profile.all, prof.obs) # add the partial results to the results
    #
    # plot 1 - map
    #
    file.map <- file.path(path.out, paste(prof, "_map.", device, sep = ""),  # name of the file
                          fsep = .Platform$file.sep)
    
    m <- basemap +                                                              # get the base map
      ggplot2::geom_path(data = prof.traj,                                      # add the trajectories
                         mapping = ggplot2::aes(x = lon, y = lat, 
                                                group = trajlabel, 
                                                colour = trajlabel)) + 
      ggplot2::geom_point(data = prof.traj[1, c("lon", "lat")],                 # add the profile point
                          mapping = ggplot2::aes(x = lon, y = lat, group = NA), 
                          shape = 10, size = 3) + 
      ggplot2::geom_point(data = prof.isec,                                     # add the intersection points
                          mapping = ggplot2::aes(x = slon, y = slat, 
                                                 group = trajlabel, 
                                                 colour = trajlabel)) + 
      #ggplot2::geom_segment(aes(x=-30,y=-40,xend=-30, yend=0), color = "blue", linetype=2, size = 1)+
      #ggplot2::geom_segment(aes(x=-59,y=13,xend=-30, yend=0), color = "blue", linetype=2, size = 1)+
      #ggplot2::geom_segment(aes(x=18.189,y=-34.352,xend=-14.4, yend=-7.967), color = "black", linetype=3, size = 1)+
      #ggplot2::geom_segment(aes(x=-14.4,y=-7.967,xend=-30, yend=0), color = "black", linetype=3, size = 1)+
      ggplot2::labs(x = "Longitude ( graus )", y = "Latitude ( graus )",  color = "trajectory" )+
      ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))
    #
    # plot 2 - cross sections
    #
    file.lonsec <- file.path(path.out, paste(prof, "_sectionlon.", device, sep = ""), fsep = .Platform$file.sep)
    file.latsec <- file.path(path.out, paste(prof, "_sectionlat.", device, sep = ""), fsep = .Platform$file.sep)
    slon <- ggplot2::ggplot(data = prof.traj, 
                            mapping = ggplot2::aes(x = lon, y = height, 
                                                   group = trajlabel, 
                                                   colour = trajlabel)) +
      ggplot2::geom_path() + ggplot2::xlim(map.xlim) +                          # add the trajectories
      ggplot2::labs(x = "longitude", y = "height", color = "trajectory") + 
      ggplot2::geom_point(data = prof.isec,                                     # add the intersections
                          mapping = ggplot2::aes(x = slon, y = sheight, 
                                                 group = trajlabel, 
                                                 colour = trajlabel)) + 
      ggplot2::geom_vline(xintercept = prof.traj[1, "lon"], linetype = "dotted") + # add the flight
      ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))
    slat <- ggplot2::ggplot(data = prof.traj, 
                            mapping = ggplot2::aes(x = lat, y = height, 
                                                   group = trajlabel, 
                                                   colour = trajlabel)) +
      ggplot2::geom_path() + ggplot2::xlim(map.ylim) +                          # add the trajectories
      ggplot2::labs(x = "latitude", y = "height", color = "trajectory") + 
      ggplot2::geom_point(data = prof.isec,                                     # add the intersections
                          mapping = ggplot2::aes(x = slat, y = sheight, 
                                                 group = trajlabel, 
                                                 colour = trajlabel)) + 
      ggplot2::geom_vline(xintercept = prof.traj[1, "lat"], linetype = "dotted") + # add the flight
      ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))
    #
    # plot 3 - profile    
    #
    if(nrow(prof.obs) > 0){
      file.profile <- file.path(path.out, paste(prof, "_profile.", device, sep = ""), fsep = .Platform$file.sep)    
      prof.df <- prof.obs[, c("file.vec", "observed", "interpolated", "background", "height")]
      prof.df["type"] <- ""
      prof.obs.df <- prof.df[, c("file.vec", "observed", "height", "type")]
      prof.int.df <- prof.df[, c("file.vec", "interpolated", "height", "type")]
      prof.bac.df <- prof.df[, c("file.vec", "background", "height", "type")]
      prof.obs.df["type"] <- "observed"
      prof.int.df["type"] <- "interpolated"
      prof.bac.df["type"] <- "background"
      colnames(prof.obs.df)[colnames(prof.obs.df) == "observed"] <- "concentration"
      colnames(prof.int.df)[colnames(prof.int.df) == "interpolated"] <- "concentration"
      colnames(prof.bac.df)[colnames(prof.bac.df) == "background"] <- "concentration"
      prof.df <- rbind(prof.obs.df, prof.int.df, prof.bac.df)
      prof.df <- prof.df[with(prof.df, order( type, -height)), ]                  # sort
      p <- ggplot2::ggplot(data = prof.df, 
                           mapping = ggplot2::aes(x = concentration, 
                                                  y = height, group = type, 
                                                  colour = type)) + 
        ggplot2::geom_path() +
        ggplot2::geom_point()
      #
      # plot
      #
      if(!is.na(plot2file)){
        if(plot2file){
          ggplot2::ggsave(filename = file.map, plot = m, device = device, width = map.width, height = map.height)                     # save the maps to files
          ggplot2::ggsave(filename = file.lonsec, plot = slon, device = device, width = sec.width, height = sec.height)
          ggplot2::ggsave(filename = file.latsec, plot = slat, device = device, width = sec.width, height = sec.height)
          ggplot2::ggsave(filename = file.profile, plot = p, device = device, width = prof.width, height = prof.height)
          filenames <- append(filenames, c(file.map, file.lonsec, file.latsec, file.profile))
        }else{
          print(m)
          print(slon)
          print(slat)
          print(p)
        }
      }
    }
  }
  # write profile data
  file.profile <- file.path(path.out, paste(basename(file.in), "_results.txt", sep = ""), fsep = .Platform$file.sep)    
  if(nrow(profile.all) > 0){
    profile.all <- profile.all[, intersect(colnames(profile.all), 
                                           c("profile", "file.vec", "site", 
                                             "lon", "lat", "height", 
                                             "year", "month", "day", "hour", "min", 
                                             "flask", "eventnumber", 
                                             "observed", "background", "interpolated", "outlier", 
                                             "filerow", "V1", "V2", 
                                             "syear", "smonth", "sday", "shour", "smin", 
                                             "V8", "V9", 
                                             "slon", "slat", "sheight", "spressure"))]
    profile.all <- profile.all[with(profile.all, order( profile, -height)), ]     # order
    rownames(profile.all) <- NULL
    #utils::write.table(profile.all, file = file.profile)
    #filenames <- append(filenames, file.profile)
  }else{
    warning("profile.all is empty")
  }
  
  return(list(filenames, profile.all))
}




#' @title Plot the trajectories grouped by year
#' @name plotTrajYear
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Plot the trajectories grouped by year
#'
#' @param file.vec     A character vector. The paths to  trajectory files
#' @param path.out     A character. The path to the folder for storing the resulting files
#' @param device       A character. Image format, i.e. PNG
#' @param map.xlim     A numeric vector. Map's min & max longitude
#' @param map.ylim     A numeric vector. Map's min & max latitude
#' @param map.height   A numeirc. Map image size
#' @param map.width    A numeirc. Map image size
#' @param stations.df  A data.frame with metereological station data. It must contain at least the columns c("name", "lon", "lat")
#' @param plot2file    A logical. Should the plot be stored as a file
#' @return             A list. The path to the created files
#' @export
plotTrajYear <- function(file.vec, path.out, device, map.xlim, map.ylim, map.height, map.width, stations.df, plot2file){
  lon <- 0; lat <- 0; filename <- 0                                             # avoid notes during package check
  #
  # process data
  #
  trajCnames <- c("V1", "V2", "year", "month", "day", "hour", "min",            # column names of the trajectory files
                  "V8", "V9", "lat", "lon", "height", "pressure")   
  traj.dat.list <- files2df(files = file.vec, header = FALSE, skip = 0,     # read all the trajectory files into a list of data.frames
                             cnames = trajCnames)
  traj.dat.list <- parallel::mclapply(1:length(traj.dat.list),                  # add file name as a column to each data.frame
                                      function(x, dat.list){
                                        dat.list[[x]]["filename"] <- names(dat.list)[x]
                                        return(dat.list[[x]])
                                      }, 
                                      dat.list = traj.dat.list)
  traj.dat <- do.call("rbind", traj.dat.list)                                  # collapse trajectory data into a single data.frame
  traj.dat["siteyear"] <- unlist(parallel::mclapply(unlist(traj.dat["filename"]),# add new column made of site and year
                                                    function(x){
                                                      paste(unlist(strsplit(x, split = "_"))[1:2], collapse = "_")
                                                    }))
  siteyear.vec <- as.vector(unlist(unique(traj.dat["siteyear"])))
  #
  # base map
  basemap <- .buildbasemap(stations.df, map.xlim, map.ylim)
  #
  # plot
  res <- list()
  for(i in 1:length(siteyear.vec)){
    dtraj.df <- traj.dat[traj.dat$siteyear == siteyear.vec[i], ]
    plot.map <- file.path(path.out, paste(siteyear.vec[i], "_map.", device, sep = ""), fsep = .Platform$file.sep)
    m <- basemap + ggplot2::geom_path(data = dtraj.df, mapping = ggplot2::aes(x = lon, y = lat, group = filename, colour = filename)) + 
      ggplot2::geom_point(data = dtraj.df[1, c("lon", "lat")], mapping = ggplot2::aes(x = lon, y = lat, group = NA), shape = 10, size = 3) + 
      ggplot2::theme(legend.position="none") + 
      ggplot2::ggtitle(label = dtraj.df[1, "siteyear"])
    if(plot2file){
      ggplot2::ggsave(filename = plot.map, plot = m, device = device, width = map.width, height = map.height)
    }else{
      print(m)
    }
    res[[i]] <- plot.map
  }
  return(res)
}



#' @title Compute the trajectory time
#' @name computeTrajTime
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Compute the trajectory time
#'
#' @param file.vec A vector of character. The paths to the input files
#' @param line.vec A vector. Ids of rows in each file in file.vec
#' @param cnames   A character. Column names of hysplit files
#' @return         A list of difftime (in days)
#' @export
computeTrajTime <- function(file.vec, line.vec, cnames){
  #cnames = HYSPLIT.COLNAMES
  data.list <- files2df(files = file.vec, header = FALSE, skip = 0, cnames = cnames)
  lapply(seq_along(data.list), function(x, data.list, line.vec){
    res <- NA
    adf <- data.list[[x]]
    l <- line.vec[x]
    if(!is.na(l)){
      date.s <- unlist(adf[l, 3:7])
      date.e <- unlist(adf[1, 3:7])
      if(date.s[1] < 100){date.s[1] <- date.s[1] + 2000}
      if(date.e[1] < 100){date.e[1] <- date.e[1] + 2000}
      date.se <- as.data.frame(rbind(date.s, date.e))
      date.se[, 2] <- formatC(date.se[, 2], width = 2, flag = 0)
      date.se[, 3] <- formatC(date.se[, 3], width = 2, flag = 0)
      date.se[, 4] <- formatC(date.se[, 4], width = 2, flag = 0)
      date.se[, 5] <- formatC(date.se[, 5], width = 2, flag = 0)
      date.se[, 6] <- rep("00", time = 2)
      date.s <- as.POSIXct(paste(paste(date.se[1, 1:3], collapse = "-"), paste(date.se[1, 4:6], collapse = ":"), sep = " "))
      date.e <- as.POSIXct(paste(paste(date.se[2, 1:3], collapse = "-"), paste(date.se[2, 4:6], collapse = ":"), sep = " "))
      res <- difftime(time1 = date.e, time2 = date.s, units = "days")
    }
    return(res)
  }, 
  data.list = data.list, 
  line.vec = line.vec
  )
}





#---- BRIEFCASE ----


#' @title Get data from briefcases
#' @name get_os
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Extract history A and C from collected report files 
#'
#' @param file.in A character. The path to a collect report file. i.e /home/user/PFP_3723_ALF_2010_02_17.txt
#' @return        A data.frame with the results of comamnds "HISTORY> A" and "HISTORY> C"
#' @export
getHistoryAC <- function(file.in){
  profile <- paste(unlist(strsplit(sub("([^.]+)\\.[[:alnum:]]+$", "\\1", 
                                       basename(file.in)), split = "_"))[3:6], collapse = "_")
  file.dat <- readLines(file.in, skipNul = TRUE, warn = FALSE)
  hs <- grep("HISTORY>", file.dat)
  # find the commands in the text
  hA <- grep("^HISTORY> A$", file.dat, ignore.case = TRUE)
  hC <- grep("^HISTORY> C$", file.dat, ignore.case = TRUE)
  if(length(hA) == 0 | length(hC) == 0){
    warning(sprintf("HISTORY A or C not found in: %s", file.in))
    pres <- cbind(as.data.frame(t(rep(NA, time = 16))), profile)
    names(pres) <- c("sample", "plan", "start", "end", "min", "max", "mean", "temperature (C)", "humidity (%RH)", "pressure (mbar)", "planmts", "startmts", "endmts", "minmts", "maxmts", "meanmts", "profile")
    return(pres)
  }
  if(length(hA) > 1 || length(hC) > 1){
    stop(sprintf("Ambiguos HISTORY tag found in: %s", file.in))
  }

  # get the text for HISTORY> A
  hA.dat <- file.dat[(hA + 1):hs[match(hA, hs) + 1]]
  hA.dat <- hA.dat[1:(length(hA.dat) - 1)]                                      # removes the last line
  hA.dat <- gsub("\\(low\\)|\\(high\\)", "", hA.dat)                            # remove instances of (low) and (high)
  # get the text for HISTORY> C
  hC.dat <- file.dat[(hC + 1):hs[match(hC, hs) + 1]]
  hC.dat <- hC.dat[1:(length(hC.dat) - 1)]
  #
  pat <- "*  |*/"                                                                # regular expression' pattern to split 
  #
  hA.dat.split <- strsplit(hA.dat, split = pat)
  hA.mat <- trimws(do.call("rbind", lapply(hA.dat.split, function(x){x <- x[x != ""]})))
  hA.df <- as.data.frame(hA.mat[2:nrow(hA.mat),], stringsAsFactors = FALSE)
  colnames(hA.df) <- trimws(hA.mat[1, ])
  hA.df <- data.matrix(hA.df)
  #
  hC.dat.split <- strsplit(hC.dat, split = pat)
  hC.mat <- trimws(do.call("rbind", lapply(hC.dat.split, function(x){x <- x[x != ""]})))
  hC.df <- as.data.frame(hC.mat[2:nrow(hC.mat),], stringsAsFactors = FALSE)
  colnames(hC.df) <- trimws(hC.mat[1, ])
  hC.df <- data.matrix(hC.df)
  # validation of new briefcase model
  if(colnames(hA.df)[length(colnames(hA.df))] != "mean"){                         # accounts for different names in columns on different models of briefcases
    colnames(hA.df)[length(colnames(hA.df))] <- "mean"
    colnames(hC.df) <- c("sample", "temperature (C)", "pressure (mbar)", "humidity (%RH)")  # re-name to old names
    hC.df <- hC.df[,   c("sample", "temperature (C)", "humidity (%RH)", "pressure (mbar)")]   # re-oprder to match the old briefcases' structure
  }
  # feet to meters
  f2m <- 0.3048
  res <- cbind(merge(hA.df, hC.df, by = "sample"))
  res["planmts"] <- as.vector(res["plan"]) * f2m
  res["startmts"] <- as.vector(res["start"]) * f2m
  res["endmts"] <- as.vector(res["end"]) * f2m
  res["minmts"] <- as.vector(res["min"]) * f2m
  res["maxmts"] <- as.vector(res["max"]) * f2m
  res["meanmts"] <- as.vector(res["mean"]) * f2m
  #
  return(cbind(res, profile))
}
