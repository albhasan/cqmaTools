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
    stop("DEPRECATED. Use filter_traj")
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
    stop("DEPRECATED")
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
    stop("DEPRECATED")
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
