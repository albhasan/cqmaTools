# util.R

#---- TODO ----
# - Check .removeHeader VS .removeHeaders
# - Check if .inInterval could be replaced by findInterval

#---- Checked ----

#' @title Read text files into data.frames
#' @name files2df
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Read text files into data.frames (one per file)
#'
#' @param file.vec A vector of character. The paths to the input files
#' @param header   A logical. Do the files have a header row?
#' @param skip     A numeric. Lines to skip from the top of the file
#' @param cnames   A vector of character. The column names of the data in the files
#' @return         A list of data.frames. The list names matches the file names
#' @export
files2df <- function(file.vec, header, skip, cnames){
  res <- list()
  if(length(file.vec) == 0){
    warning("Empty list")
    return(res)
  }
  for(i in 1:length(file.vec)){
    res[[i]] <- .file2df(
      file.in = file.vec[i],
      cnames = cnames, 
      header = header, 
      skip = skip)
  }
  names(res) <- basename(file.vec)
  return(res)
}



#' @title Get the data from profile name
#' @name getDataFromProfile
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Get the data from profile name
#'
#' @param prof.name A character. The name of the profile. i.e. RBA_2016_05_16
#' @return         A data.frame with columns c("site", "year", "month", "day")
#' @export
getDataFromProfile <- function(prof.name){
  nameelements <- lapply(prof.name, function(x){
    return(unlist(strsplit(x, split = "_")))
  })
  nel.df <- as.data.frame(do.call("rbind", nameelements), stringsAsFactors = FALSE)
  names(nel.df) <- c("site", "year", "month", "day")
  return(nel.df)
}


#' @title Get the operative system's name
#' @name get_os
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Get the operative system's name. Taken from https://www.r-bloggers.com/identifying-the-os-from-r/
#'
#' @return                   A single string
#' @export
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}





#' @title Convert a data frame to a string
#' @name df2text
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Convert a data frame to a length-1 character vector
#'
#' @param a.df A data.frame
#' @return     A length-1 character vector
#' @export
df2text <- function(a.df){
  t <- paste(colnames(a.df), collapse = " ")
  b <- paste(apply(a.df, 1 , paste, collapse = " "), collapse = "\n")
  return(paste(c(t, "\n", b), collapse = ""))
}



# Test in which interval are the given value 
#
# @param val  A numeric vector of the values to test
# @param vec  A numeric vector with the limits of the consecutive intervals. Its length must be greater than 1
# @return     A boolean matrix. The columns represent the interval's limits (vec) and the rows the values (val). The number of intervals is the number of element in vec minus one
.inInterval <- function(val, vec){
  # TODO: replace by findInterval {base}
  if(is.null(val)){return(matrix(ncol = 2, nrow = 0))}
  int.mat <- matrix(NA, ncol = 2, nrow = length(vec) - 1, byrow = TRUE) # interval matrix
  # build a test matrix
  for(i in 2:length(vec)){
    int.mat[i - 1, ] <- c(vec[i - 1], vec[i])
  }
  res <- lapply(val, function(x, mat){return(x >= mat[, 1] & x <= mat[, 2])}, mat = int.mat)
  res <- matrix(unlist(res), ncol = nrow(int.mat), nrow = length(val), byrow = TRUE)
  colnames(res) <- paste(rep("int", nrow(int.mat)), 1:nrow(int.mat), sep = "")
  rownames(res) <- paste(rep("val", length(val)), 1:length(val), sep = "")
  return(res)
}





# Check if the given coordinates fall in the limits. xy.df is in if a limit is NA  
#
# @param xy.df  A data.frame. The coordinates to test
# @param minx   A numeric. The minimum X
# @param maxx   A numeric. The maximum X
# @param miny   A numeric. The minimum Y
# @param maxy   A numeric. The maximum Y
.inbound <- function(xy.df, minx, maxx, miny, maxy){
  xy.df[, "keep"] <- rep(TRUE, times = nrow(xy.df))
  if(!is.na(minx)){xy.df[, "keep"] <- xy.df[, "keep"] & xy.df[, "x"] >= minx}
  if(!is.na(maxx)){xy.df[, "keep"] <- xy.df[, "keep"] & xy.df[, "x"] <= minx}
  if(!is.na(miny)){xy.df[, "keep"] <- xy.df[, "keep"] & xy.df[, "y"] >= miny}
  if(!is.na(maxy)){xy.df[, "keep"] <- xy.df[, "keep"] & xy.df[, "y"] <= miny}
  return(as.vector(unlist(xy.df[, "keep"])))
}




#' @title Remove header from files
#' @name removeHeaders
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Remove header from files
#'
#' @param file.vec A character vector. The paths to the input files
#' @param path.out A length-1 character. The path to the folder for storing the resulting files
#' @param skip     A length-1 numeric. Number of lines to remove from the beginning of each file
#' @param cnames   A character. The names of the columns of the returned data frame
#' @return         A list of paths to the created files
#' @export
removeHeaders <- function(file.vec, path.out, skip, cnames){
  #cnames <- HYSPLIT.COLNAMES                                                    # column names of hysplit files
  file.dat.list <- files2df(file.vec = file.vec,  header = FALSE, 
                             skip = skip, cnames = cnames)
  res <- list()
  for (i in 1:length(file.dat.list)) {
    file.dat <- file.dat.list[[i]]
    newfile <- file.path(path.out, names(file.dat.list)[[i]], fsep = .Platform$file.sep)  
    utils::write.table(file.dat, file = newfile, col.names = FALSE, row.names = FALSE, quote = FALSE)
    res[[i]] <- newfile
  }  
  return(res)
}


















#---- UN-Checked ----





# Replace outliers in trajectories' interpolated values using ...
#
# @param data.vec   A numeric vector. The trajectory data interpolated for a single profile
# @param nsd        A numeric. Number of standard deviation from the central tendency
# @param maxfm.ppm  A numeric. Maximum number of units away from the central tendency measure
# @return           A data.frame with two columns: The new values (background) and booleans indicating those values replaced
.background.softrules <- function(data.vec, nsd, maxfm.ppm){
  outlier <- as.data.frame(cbind(data.vec, .isOutlier(data.vec = data.vec, nsd = nsd, maxfm.ppm = maxfm.ppm, use.median = TRUE))) #  identify outliers
  names(outlier) <- c("background", "outlier")
  outlier$outlier <- as.logical(outlier$outlier)                              # cast back to logical (R sucks!)
  # find stand-alone outliers
  newoutlier <- outlier$outlier
  for(i in 2:(length(outlier$outlier) - 1)){
    if(identical(outlier$outlier[(i-1):(i+1)], c(FALSE, TRUE, FALSE))){
      outlier$background[i] <- NA
    }else{
      newoutlier[i] <- FALSE
    }
  }
  outlier$outlier <- newoutlier
  outlier$outlier[is.na(data.vec)] <- TRUE                                    # mark missing values as outliers
  outlier$background <- .fillNAs(outlier$background)                          # replace NAs with local means
  return(outlier)
}


# Replace the NAs in a vector using the a local mean
#
# @param data.vec   A numeric vector including NAs
# @return           A numeric vector
.fillNAs <- function(data.vec){
  res <- data.vec
  lastobs <- NA
  nextobs <- NA
  for(i in 1:length(res)){
    if(is.na(res[i])){
      if((i + 1) <= length(res)){                                               # get next valid observation
        for(j in (i + 1):length(res)){
          if(!is.na(res[j])){
            nextobs <- res[j]
            break
          }
        }
      }
      res[i] <- mean(c(lastobs, nextobs), na.rm = TRUE)
    }else{
      lastobs <- res[i]                                                         # actual becones last valid observation
    }
  }
  return(res)
}

# Mark the outliers in the given data vector
#
# @param data.vec   A numeric vector. The data to test
# @param nsd        A numeric. The number of standard deviations 
# @param nsd        A logical. Must the median be used instead of the mean?
# @param maxfm.ppm  A numeric. Maximum number of units away from the central tendency measure
# @return           A logical vector
.isOutlier <- function(data.vec, nsd, maxfm.ppm, use.median){
  dm <- mean(data.vec, na.rm = TRUE)
  if(use.median){dm <- stats::median(data.vec, na.rm = TRUE)}
  dsd <- stats::sd(data.vec, na.rm = TRUE)
  testsd <- data.vec < (dm - nsd * dsd) | data.vec > (dm + nsd * dsd)
  testlim <- data.vec < (dm - maxfm.ppm) | data.vec > (dm + maxfm.ppm)
  return(testsd | testlim)
}






# return the area of a rectangular trapeze |=/
#
# @param height       A numeric. The height of trapeze
# @param minorlength  A numeric. The minor side
# @param mayorlength  A numeric. The mayor side
# @return             A numeric. The are of the trapeze
.recttrapezearea <- function(height, minorlength, mayorlength){
  a <- height * (minorlength + mayorlength)/2
  return(a)
}





# join column vectors and build dates using row vectors
.col2time <- function(ayear, amonth, aday, ahour, aminute, asecond, atimezone){
  res <- rep(NA, times = length(ayear))
  for(x in 1:length(ayear)){
    if(is.na(ayear[x])){next}
    first <- paste(ayear[x], 
                   formatC(amonth[x], width = 2, flag = 0), 
                   formatC(aday[x], width = 2, flag = 0), sep = "-")
    second <- paste(formatC(ahour[x], width = 2, flag = 0), 
                    formatC(aminute[x], width = 2, flag = 0), 
                    formatC(asecond[x], width = 2, flag = 0), sep = ":")
    third <- paste(first, second, sep = " ")
    res[x] <- as.POSIXct(third, tz = atimezone)  
  }
  return(as.POSIXct(res, origin = "1970-01-01", tz = atimezone))
}



# Cast factor to numeric (R is still a motherfucking piece of shit!)
#
# @param x  A factor
# @return   A numeric vector
.as.numeric.factor <- function(x){as.numeric(levels(x))[x]}



# Given a list of data.frames, this function adds the list names as columns on each of the data.frames
#
# @param df.list  A list of data.frames
# @return         A list of data frames
.listname2data.frame <- function(df.list, colname){
  res <- parallel::mclapply(1:length(df.list), 
                            function(x, df.list){
                              df.list[[x]][colname] <- names(df.list)[x]
                              return(df.list[[x]])
                            }, 
                            df.list = df.list
                            )
  return(res)
}






#' Read a text file made of observations
#'
#' @param file.in  A character. Path to a text file with data
#' @param header   A logical. Does the first column contain column names?
#' @param skip     A numeric. Number of lines to skip
#' @param cnames   A vector character. The names of the columns
#' @return         A data.frame
#' @export
file2df <- function(file.in, header, skip, cnames){
  .file2df(file.in, header, skip, cnames)
}

.file2df <- function(file.in, header, skip, cnames){
  file.dat <- utils::read.table(file = file.in, sep = "", header = header, 
                                skip = skip, stringsAsFactors = FALSE)
  if (!header)
    colnames(file.dat) <- cnames
  return(file.dat)
}



# Filter a data.frame by columns and a row
#
# @param df         A data.frame.
# @param keepCols   A character vector. The column names to keep in the result data.frame
# @param flagName   A character. The name of the column with the flags
# @param keepFlags  A character vector. The flags which mark the rows to keep
# @return           A filtered data.frame
.filterDataframe <- function(df, keepCols, flagName, keepFlags){
  df <- df[which(df$flag %in% keepFlags), ] # filter using flags
  df <- df[keepCols]                    # subset and reorder columns
  return(df)
}



# Get the week number respect the month of the given date
#
# @param adate        A charactrer in a format parseable by as.POSIXlt
# @param weekfirstday A numeric value indicating which is the first day of the week. The defaul is 1, that is Monday
# @param tz           A character representing a time zone. The default is GMT
# @return             A numeric value representing the week of the month of the given date. The first week is the number 1
.monthWeek <- function(adate, weekfirstday = 1, tz = "GMT"){
  d <- as.POSIXlt(adate, tz = tz)           # tested date
  s <- as.POSIXlt(paste(strftime(d, "%Y"), strftime(d, "%m"), "01", sep = "-"), tz = tz) # first day of the month
  tdif <- as.numeric(d - s)                 # dates difference in days
  ddow <- strftime(s, "%u")                 # day of the week of first day of the month
  if(7 - as.numeric(ddow) >= tdif){return(1)} # first week
  wday <- vector()
  start <- as.numeric(ddow) + 1
  for(i in 1:tdif){
    wday <- append(wday, start)
    start <- start + 1
    if(start > 7) start <- 1
  }
  return(as.vector(table(wday)[weekfirstday] + 1))
}







# Transforms a decimal year date to a date-format string
#
# @param year.dec A number repesenting a date as a decimal year. e.g. 2000.0013661202
# @return         A character. e.g. 2000-01-01 11:59:59.999410
.ydec2date <- function(year.dec){
  return(as.character(lubridate::date_decimal(year.dec)))
}



# Transforms a date decimal year. 
# NOTE: use formatC to get more decimals from the result. i.e formatC(.date2ydec("2000-01-01 11:59:59.999410"), digits = 10, format="f")
#
# @param aDate    A date in a format supported by as.POSIXlt, i.e "2000-01-01 11:59:59.999410"
# @param timezone A character. The time zone. i.e. "GMT"
# @return         A number e.g. 2000.0013661202
.date2ydec <- function(adate, timezone){
  daysperyear <- 365 # 365.25
  d <- as.POSIXlt(adate, tz = timezone)
  y <- d$year + 1900
  leap <- .isLeapYear(y)
  firstday <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366) + as.numeric(c(FALSE, rep(leap, 12))) # first day of each month
  return(y + (((firstday[(d$mon + 1)] - 1) * 24 * 3600) + ((d$mday - 1) * 24 * 3600) + (d$hour * 3600) + (d$min * 60) + d$sec) / ((daysperyear + as.numeric(leap)) * 24 * 3600))
}



# Check wether the given year is leap or not
#
# @param year A numeric vector
# @return A boolean vector
.isLeapYear <- function(year){
  (year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0))
}



# Interpolate the value of a point over a line
#
# @param line.xyv A line represented by a 2-row 3-column matrix (x, y value)
# @param point.xy A point on the line represented by 2-element vector (x, y)
# @return         A number. The interpolated value for the point
.lineInterpolation <- function(line.xyv, point.xy){
  dl <- as.vector(stats::dist(line.xyv[, 1:2]))                                 # line's length
  hl <- line.xyv[2, 3] - line.xyv[1, 3]                                         # value interval
  dp1 <- as.vector(stats::dist(rbind(line.xyv[1, 1:2], point.xy)))              # distance from line' start to point
  dp2 <- as.vector(stats::dist(rbind(line.xyv[2, 1:2], point.xy)))              # distance from line' end to point
  dp <- dp1/(dp1 + dp2)                                                         # normalized distance from line' start to point
  return(line.xyv[1, 3] + (dp * hl))
}




# =======================================================================================
# PACKAGE RELATED 
# =======================================================================================






# Check coordinates for positive or negative missmatches
#
# @param x        A data.frame with coordinates in columns c("lon", "lat")
# @param nsd      A numeric. Number of standard deviations to classify ourliers
# @param treshold A numeric. The minimum difference (as a proportion in SDs) to accept a sign change in coordinates
# @return         A data.frame with corrected coordinates and a logical indicating those which changed, i.e.  c("lon", "lat", "changed")
.checklonlat <- function(x, nsd, treshold){
  #-----------------------------------------------------------------------------
  # workaround for TEF
  #-----------------------------------------------------------------------------
  if(is.data.frame(x) == FALSE){
    if(is.list(x)){
      if(length(x) == 1){
        if(is.data.frame(x[[1]])){
          x <- x[[1]]
        }
      }
    }
  }
  #-----------------------------------------------------------------------------
  ll.df <- x[, c("lon", "lat")]
  mlon <- mean(ll.df$lon)
  mlat <- mean(ll.df$lat)
  sdlon <- stats::sd(ll.df$lon)
  sdlat <- stats::sd(ll.df$lat)
  ll.df["outlon"] <- ll.df$lon < (mlon - (sdlon * nsd)) | ll.df$lon > (mlon + (sdlon * nsd))
  ll.df["outlat"] <- ll.df$lat < (mlat - (sdlat * nsd)) | ll.df$lat > (mlat + (sdlat * nsd))
  
  nll.df <- ll.df[, c("lon", "lat")]
  nll.df[ll.df$outlon,"lon"] <- ll.df[ll.df$outlon, "lon"] * -1
  nll.df[ll.df$outlat,"lat"] <- ll.df[ll.df$outlat, "lat"] * -1
  sdnlon <- stats::sd(nll.df$lon)
  sdnlat <- stats::sd(nll.df$lat)
  
  lon <- ll.df$lon
  lat <- ll.df$lat
  changed <- rep(FALSE, nrow(x))
  
  # replace wrong values   
  if(sdnlon != 0 && sdnlat != 0){
    if(sdlon / sdnlon > treshold){
      lon <- nll.df$lon
      changed <- changed | ll.df$outlon
    }
    if(sdlat / sdnlat > treshold){
      lat <- nll.df$lat
      changed <- changed | ll.df$outlat
    }
    
  }
  return(data.frame(lon, lat, changed))
}



# Get a GDAS file name from a row of a traj_spec file
#
# @param trajspecs.row  A data.frame with a single row of a traj_spec file. It must contain at least the columns c("yr", "mo", "da")
# @param                A character with the name of a GDAS file. i.e. "gdas1.feb15.w4"
.buildGDASfilename <- function(trajspecs.row, timezone){
  # get the week of the month
  adate  <- paste(unlist(trajspecs.row["yr"]), unlist(trajspecs.row["mo"]), 
                  unlist(trajspecs.row["da"]), sep = "-")
  wom <- .monthWeek(adate = adate, weekfirstday = 1, tz = timezone)
  adate <- as.POSIXlt(adate, tz  = timezone)
  # build the filename
  m <- tolower(month.abb[unlist(trajspecs.row["mo"])])
  return(paste("gdas1.", m, strftime(adate, "%y"), ".w", wom, sep = ""))
}



# Run HYSPLIT simulations for the traj_spec files in the given path
#
# @param path.in            A character representing a folder with traj_spec files
# @param backTrajTime       A numeric. Time to modify the hysplit file search. For example, 10 days to the past is (10 * 24 * 3600) * (-1)
# @param hysplit.exec.path  A character. The path to the HYSPLIT's exec folder.
# @param hysplit.work.path  A character. The path to the HYSPLIT's working folder.
# @param path.out           A character. The path to store the resulting files. Usually HYSPLIT's simulations folder.
# @param timezone           A character. The time zone used for date computations.i.e. GMT
# @return                   A character list of the created files
.runsimulation <- function(path.in, backTrajTime, hysplit.exec.path, 
                           hysplit.work.path, path.out, timezone){
  exec.file <- file.path(hysplit.exec.path, "hyts_std", 
                         fsep = .Platform$file.sep)
  if(!file.exists(exec.file)){
    stop("ERROR: Hysplit's executables not found")
  }
  files.in  <- list.files(path = path.in, full.names = TRUE)
  for(i in 1:length(files.in)){
    file.out <- .runsim(file.in = files.in[i], 
                        backTrajTime = backTrajTime, 
                        MDL = hysplit.exec.path, 
                        MET = hysplit.work.path, 
                        SIM = path.out, 
                        timezone = timezone)
    files.out[[i]] <- file.out
  }  
  return(files.out)
}



# Run HYSPLIT simulation for a single traj_specs file and store the results
# This function replaces the bash script batch.sh
#
# @param file.in      A character. The path to a traj_specs_* file
# @param backTrajTime A numeric. Time to modify the hysplit file search. For example, 10 days to the past is (10 * 24 * 3600) * (-1)
# @param MDL          A character. The path to the HYSPLIT's exec folder.
# @param MET          A character. The path to the HYSPLIT's working folder.
# @param SIM          A character. The path to the HYSPLIT's simulations folder.
# @param timezone     A character. The time zone used for date computations.i.e. GMT
# @return             A character list with the result files
# @example
# ## filter raw data
# #rawdatafiles.path <- "/home/alber/Documents/Dropbox/alberLocal/inpe/cqma/data/01"
# #rawDataClean.path <- "/home/alber/Documents/Dropbox/alberLocal/inpe/cqma/data/03/test"
# #rawDataClean.files <- unlist(.splitRawdata(path.in = rawdatafiles.path, path.out = rawDataClean.path))
# #backTrajTime <- (10 * 24 * 3600) * (-1)
# #MDL <- "/Users/manuel/Documents/Models/Hysplit4/exec/"
# #MET <- "/Users/manuel/Documents/Models/Hysplit4/working/"
# #SIM <- "/Users/manuel/Documents/Models/Hysplit4/simulations/"
# #.runsims(file.in = rawDataClean.files[1], MDL = MDL, MET = MET, SIM = SIM)
.runsim <- function(file.in, backTrajTime, MDL, MET, SIM, timezone){
  cnames <- c("site", "lat", "lon", "height", "yr", "mo", "da", "hr", "mm", "flask")
  file.dat <- .file2df(file.in = file.in, header = FALSE, skip = 0, cnames = cnames)
  # format columns
  file.dat["mof"] <- sprintf("%02d", unlist(file.dat["mo"]))
  file.dat["daf"] <- sprintf("%02d", unlist(file.dat["da"]))
  file.dat["hrf"] <- sprintf("%02d", unlist(file.dat["hr"]))
  file.dat["mmf"] <- sprintf("%02d", unlist(file.dat["mm"]))
  # get the file names of the metereological data file
  met_files <- unlist(lapply(1:nrow(file.dat), function(x, file.dat){return(.buildGDASfilename(file.dat[x, ], timezone = timezone))}, file.dat = file.dat))
  # get the file names of the metereological data file backTrajTime seconds before the trajectory
  met_filesp1 <- unlist(lapply(1:nrow(file.dat), 
                               function(x, file.dat, backTrajTime, timezone){
                                 # get row's date
                                 adate <- as.POSIXlt(paste(unlist(file.dat[x, ]["yr"]), 
                                                           unlist(file.dat[x, ]["mo"]), 
                                                           unlist(file.dat[x, ]["da"]), 
                                                           unlist(file.dat[x, ]["hr"]), 
                                                           unlist(file.dat[x, ]["mm"]), 
                                                           sep = "-"), tz = timezone)
                                 # date backTrajTime seconds before or after the trajectory
                                 adate <- adate + backTrajTime
                                 # replace the old date
                                 tmprow <- file.dat[x, ]
                                 tmprow["yr"] <- as.numeric(strftime(adate, "%Y"))
                                 tmprow["mo"] <- as.numeric(strftime(adate, "%m"))
                                 tmprow["da"] <- as.numeric(strftime(adate, "%d"))
                                 tmprow["hr"] <- as.numeric(strftime(adate, "%H"))
                                 tmprow["mm"] <- as.numeric(strftime(adate, "%M"))
                                 # build the file names
                                 return(.buildGDASfilename(tmprow, timezone = timezone))
                               }, file.dat = file.dat, backTrajTime = backTrajTime, timezone = timezone))
  #
  control.files <- apply(cbind(file.dat, met_files, met_filesp1), 1, .runsim.buildcontrol, MET = MET)
  res <- list()
  for(i in 1:length(control.files)){
    # 1 - create CONTROL file
    write(control.files[[i]], file = file.path(MDL, "CONTROL",                  # write a CONTROL file to directory where trajectory code executable resides
                                               fsep = .Platform$file.sep)) 
    # 2 - go to exec directory and run simulation   
    row.file.dat <- file.dat[i ,]
    file.out <- paste(as.character(unlist(row.file.dat["site"])), 
                      row.file.dat["yr"], row.file.dat["mof"], 
                      row.file.dat["daf"], row.file.dat["hrf"], 
                      row.file.dat["height"], sep = "_")
    print(paste("Simulation:", file.out, "\n"))
    system(file.path(MDL, "hyts_std", fsep = .Platform$file.sep), wait = TRUE)
    # 3 rename and move to trajectory storage directory
    path.file.res <- file.path(MDL, file.out, fsep = .Platform$file.sep)
    path.file.out <- file.path(SIM, file.out, fsep = .Platform$file.sep)
    system(paste("mv ", path.file.res, path.file.out), wait = TRUE)
    res[[i]] <- path.file.out
  }
  return(res)
}



# Build the content of a hysplit control file
#
# @param x
# @param MET  A character. The path to the HYSPLIT's working folder.
# @return     A character. The contents of a hysplit CONTROL file
.runsim.buildcontrol <- function(x, MET){
  # Explanation
  #------------
  # Starting time                               year month day hour
  # Number of starting locations                N
  # Starting location                           lat  lon  height(m)
  # Total runtime                               (+-) M (hr)
  # Vertical motion option                      0 data   1 isob   2 risen  3 ...
  # Top of model domain (m)                     e.g. 10000.0
  # Number of simultaneous input met fields     usually 1
  # Meteorological data grid # 1 directory      \main\sub\data\
  # Meteorological data grid # 1 filename       file_name
  
  # Example 
  #--------
  # 13 03 18 00        # Starting time
  # 1                  # Number of starting locations
  # 40.0 -90.0 1500.0  # lat lon height(m)
  # -48                # Total runtime
  # 0                  # Vertical motion option
  # 10000.0            # Top of model domain (m)
  # 1                  # Number of simultaneous input met fields
  # ../working/        # Meteorological data grid # 1 directory
  # RP201303.gbl       # Meteorological data grid # 1 filename
  # ./
  # tdump_001
  paste(
    paste(x["yr"], x["mof"], x["daf"], x["hrf"], "\n"),
    paste(1, "\n"),
    paste(x["lat"], x["lon"], x["height"], "\n"),
    paste(-240, "\n"),
    paste(0, "\n"),
    paste("10000.0", "\n"),
    paste(2, "\n"),
    paste(MET, "\n"),
    paste(x["met_files"], "\n"),
    paste(MET, "\n"),
    paste(x["met_filesp1"], "\n"),
    paste("./", "\n"),
    paste("tdump_001", "\n"),
    sep = ""
  )
}












# Build a SpatialLines object from a trajectory data.frame. Each line is identified by its first row in the data.frame
#
# @param traj.dat A data.frame with the trajectory. It uses the format of Hysplit output files
# @param crs      A CRS object. It is the coordinate reference system of the data
# @return         A SpatialLines object.
.traj2spLines <- function(traj.dat, crs){
  Lines.list <- list()
  for (i in 2:nrow(traj.dat)){
    l <- sp::Line(rbind(traj.dat[i - 1,][c("lon", "lat")], 
                        traj.dat[i,][c("lon", "lat")]))
    Lines.list[[i - 1]] <- sp::Lines(l, as.character(i - 1))
  }
  return(sp::SpatialLines(Lines.list, proj4string = crs))
}











# Helper function for crossdata. Take the trajectories data and interpolates its matching value according to observations
#
# @param x                  A number indexing the current trajectory
# @param traj.records       A data.frame of the trajectories' first rows over the sea. One row per trajectory
# @param traj.data.match    A boolean matrix of the traj.records versus intervals. The intervals' limits are the ordered latitudes of the stations
# @param stations           A data.frame made of the name, longitude, latitude, and data file path of each of station. i.e c("name","lon", "lat", "file"). The data frame must be ordered by latitude to match the intervals in traj.data.match
# @param stations.dat       A list of the data on each file of stations["file"]
# @param searchTranslation  A number used to modify records' search on station data (in seconds).
# @param timezone           A character. The time zone. i.e. "GMT"
# @param tolerance.sec      A numeric. A tolerance used when comparing dates
# @return                   A number resulting from interpolating the station's data to the latitude of traj.records[x, ] 
.crossdata.aux <- function(x, traj.records, traj.data.match, stations, stations.dat, searchTranslation, timezone, tolerance.sec){
  DATE.COLNAMES <- c("year", "month", "day", "hour", "min")
  TIME.ZONE <- "GMT"
  res <- NA
  # get trajectory's date
  d <- unlist(traj.records[x, DATE.COLNAMES])
  if (all(is.na(d)))
      return(NA)
  
  trajdate <- paste(
    paste(d[DATE.COLNAMES[1]] + 2000, 
          sprintf("%02d", d[DATE.COLNAMES[2]]), 
          sprintf("%02d", d[DATE.COLNAMES[3]]), sep = "-"), 
    paste(sprintf("%02d", d[DATE.COLNAMES[4]]), 
          sprintf("%02d", d[DATE.COLNAMES[5]]), "00", sep = ":"),
    sep = " "
  )
  d.noon <- paste(strsplit(trajdate, split = " ")[[1]][1], "12:00:00", sep = " ") # approximate date to noon
  trajdate <- as.POSIXct(d.noon, tz = TIME.ZONE)
  trajdate <- trajdate + searchTranslation                                        # modify the search date by searchTranslation seconds
  # get the matching stations
  interval <- match(TRUE, traj.data.match[x, ])                                   # get the first interval in which the trajectory record falls
  if(!is.na(interval)){
    stations.match <-  stations[interval:(interval + 1), ]
    # cast character to date
    stations.dat.match <- stations.dat[interval:(interval + 1)]
    stations.dat.match[[1]]["date"] <- as.POSIXct(unlist(stations.dat.match[[1]]["date"]), tz = timezone)
    stations.dat.match[[2]]["date"] <- as.POSIXct(unlist(stations.dat.match[[2]]["date"]), tz = timezone)
    # filter stations's data by date
    stations.rows <- rbind( # first row matching the trajectory date on each station data file
      stations.dat.match[[1]][stations.dat.match[[1]]$date > (trajdate - tolerance.sec) & stations.dat.match[[1]]$date < (trajdate + tolerance.sec), ][1,], 
      stations.dat.match[[2]][stations.dat.match[[2]]$date > (trajdate - tolerance.sec) & stations.dat.match[[2]]$date < (trajdate + tolerance.sec), ][1,]  
    )
    if(sum(is.na(stations.rows)) == 0){
      toInterpol <- cbind(stations.match, stations.rows)  
      res <- .lineInterpolation(
        line.xyv = as.matrix(toInterpol[, c("lon", "lat", "V2")]), 
        point.xy = traj.records[x, c("lon", "lat")]
      )
    }
  }
  # interpolate toInterpol' latitude to get the value on record's latitutde
  return(res)
}















# Add a column with a human-readable date to a file with metereological data
#
# @param file.in  A character. Path to a metereological station data file
# @param file.out A character. Path to the resulting file
.addData2Station <- function(file.in, file.out){
  cnames <- c("datedec", "concentration")
  file.dat <- .file2df(file.in = file.in, header = FALSE, skip = 0, cnames = cnames)
  file.dat["normdec"] <- unlist(lapply(unlist(file.dat["datedec"]), function(x){return(.ydec2date(x))}))
  utils::write.csv2(file.dat, file = file.out, quote = TRUE)
}



# Write intersection list to a file
#
# @param traj.intersections A list of two obejcts. A character vector and a list of data.frames. The data.frames are made of the rows in the matching trajectory file which first crossed to the sea
# @param file.out           A character. The path to the resulting file
.writeTrajIntersections <- function(traj.intersections, file.out){
  filepaths <- traj.intersections[[1]]
  filerows <- traj.intersections[[2]]
  row.df <- do.call("rbind", filerows)
  res <- cbind(filepaths, row.df)
  utils::write.csv2(res, file = file.out)
}



# Get the metadata from file trajectories' file names adn add the profile as a colum
#
# @param trajfile.list  A character vector. The names of trajectory files
# @param cnames  A character vector. Metadata included in the trajectory's file names
# @return               A data frame
.trajFilenames2metadata <- function(file.vec, cnames){
  #cnames <- TRAJ.FILENAMES.METADATA
  bn <- basename(file.vec) 
  res <- as.data.frame(
    do.call("rbind", 
            parallel::mclapply(bn, 
                               function(x){unlist(strsplit(x, split = "_"))})
    ))
  colnames(res) <- cnames                                      # add column names to trajectory filenames' metadata 
  return(res)  
}



# Match the number of rows between interpolated and observed data. The missing interpolated values are filled using the median
#
# @param trProf.df      A data.frame of interpolated data from trajectories
# @param data.df        A data.frame of observed data
# @return A data.frame  A modified version of trProf.df with additioanl rows to match data.df
.matchData2Interpolation <- function(trProf.df, data.df){
  if(is.null(trProf.df) | is.null(data.df)){return(NA)}
  if(nrow(data.df) > nrow(trProf.df)){
    miss <- !(unlist(data.df["height"]) %in% unlist(trProf.df["height"]))
    if(sum(miss > 0)){
      mheights <- as.vector(unlist(data.df[miss, "height"]))
      nr <- nrow(trProf.df)
      tmedian <- stats::median(unlist(trProf.df["concentration"]), na.rm = TRUE)
      for(j in 1:length(mheights)){
        trProf.df[nr + j, ] <- trProf.df[nr, ]
        trProf.df[nr + j, "height"] <- mheights[j]
        trProf.df[nr + j, "interpolvalue"] <- NA
        trProf.df[nr + j, "notes"] <- "missing"
        trProf.df[nr + j, "concentration"] <- tmedian
      }
    }else{
      # NOTE: when there are two measurements for the same height
      # identify duplicated heights
      t <- as.data.frame(table(as.vector(unlist(data.df["height"]))), 
                         stringsAsFactors = FALSE)
      t[, 1] <- as.numeric(t[, 1])
      for(i in 1:nrow(t)){
        if(t[i, "Freq"] > 1){
          n <- t[i, "Freq"] - 1
          # find the matching interpolation
          trProf.rowid <- match(t[i, 1], unlist(trProf.df["height"]))
          trProf.row <- trProf.df[trProf.rowid, ]
          for(i in 1:n)
            trProf.df <- rbind(trProf.df, trProf.row)
        }
      }
    }
  }else if(nrow(trProf.df) > nrow(data.df)){
    amatch <- unlist(trProf.df["height"]) %in% unlist(data.df["height"])
    trProf.df <- trProf.df[amatch, ]
  }
  return(trProf.df)
}


# Plot a world map with trajectories
#
# @param dtraj.df A data.frame of the trajectories of the same profile
# @param map.xlim A numeric vector. The minimum and maximum in the X axes
# @param map.ylim A numeric vector. The minimum and maximum in the Y axes
# @param file.out A character. The path to the resulting file
# @param device   A character. The type of output. i.e. "png"
# @param width    A numeric. The width of the file
# @param height   A numeric. The height of the file
# @param leg      A logical. Include legend?
.plotmap <- function(dtraj.df, map.xlim, map.ylim, file.out, device, width, height, leg){
  lon <- 0; lat <- 0; filename <- 0; group <- 0; long <- 0; type <- 0           # avoid notes during package check
  p1 <- ggplot2::ggplot(data = ggplot2::map_data(map = "world"), 
                        mapping = ggplot2::aes(long, lat, group = group)) +
    ggplot2::geom_polygon(fill = "white", colour = "black") +
    ggplot2::geom_path(data = dtraj.df, mapping = ggplot2::aes(x = lon, y = lat, group = filename, colour = filename)) + 
    ggplot2::coord_quickmap(xlim = map.xlim, ylim = map.ylim, expand = TRUE) + 
    ggplot2::labs(x = "longitude", y = "latitude", color = "trajectory")
  if(!leg){  
    p1 <- p1 + ggplot2::theme(legend.position="none")
  }
  ggplot2::ggsave(filename = file.out, plot = p1, device = device, width = width, height = height)
}



# Plot the input data into map, section, and profile graphs. These plots are stored in disk
#
# @param stations.df  A data.frame with metereological station data. It must contain at least the columns c("name", "lon", "lat")
# @param map.xlim     A numeric vector. Map's min & max longitude
# @param map.ylim     A numeric vector. Map's min & max latitude
# @return             A ggplot2 object
.buildbasemap <- function(stations.df, map.xlim, map.ylim){
  lon <- 0; lat <- 0; long <- 0; group <- 0; name <- 0                         # avoid notes during package check
  basemap <- ggplot2::ggplot(data = ggplot2::map_data(map = "world"), 
                             mapping = ggplot2::aes(long, lat, group = group)) +
    ggplot2::geom_polygon(fill = "white", colour = "black") +
    ggplot2::geom_point(data = stations.df, mapping = ggplot2::aes(x = lon, y = lat, group = name), shape = 17, size = 3) + 
    ggplot2::geom_label(data = stations.df, mapping = ggplot2::aes(x = lon, y = lat, group = name, label = name, hjust = c(1, 0, 0), vjust = c(0, 0, 1))) + 
    ggplot2::coord_quickmap(xlim = map.xlim, ylim = map.ylim, expand = TRUE) + 
    ggplot2::labs(x = "longitude", y = "latitude", color = "trajectory")
}





# Add profile to trajectory data
#
# @param traj.dat.list  A list of data frames. Each data.frame is a trajectory
# @return               A list of data frames. Each data frame is a trajectoryy with additional data
.addProfile2Trajectories <- function(traj.dat.list){
  traj.dat.list <- .listname2data.frame(df.list = traj.dat.list,                # add file name as a column to each data.frame
                                        colname = "filename")
  traj.dat <- do.call("rbind", traj.dat.list)                                   # collapse trajectory data into a single data.frame
  traj.dat["profile"] <- .filename2profile(unlist(traj.dat["filename"]))         # add profile as new column to trajectory data
  return(split(traj.dat, traj.dat$profile))                                     # split trajectory data.frame by profile. One data.frame per profile
}



# Build a profile form a vector of trajectory file names
#
# @param file.vec A vector of character
# @return         A character vector
.filename2profile <- function(file.vec){
  bn <- basename(file.vec)
  return(as.vector(sapply(bn, function(x){
    toupper(paste(unlist(strsplit(x, split = "_"))[1:4], collapse = "_"))
  })))
}



# Format the file name of the trajectories to put the height before the hour
#
# @param file.vec A character vector. The names of the trajectory files
# @return A character vector
.formatTrajname <- function(file.vec){
  file.vec.list <- strsplit(file.vec, split = "_")
  file.vec.list.df <- as.data.frame(do.call("rbind", file.vec.list))
  file.vec.list.df["V6"] <- as.numeric(as.vector(unlist(file.vec.list.df["V6"])))
  file.vec.list.df["V6"] <- formatC(as.vector(unlist(file.vec.list.df["V6"])), 
                                    digits = 1, width = 6, format = "f", 
                                    flag = "0")
  file.vec.list.df <- file.vec.list.df[, c("V1", "V2", "V3", "V4", "V6", "V5")]
  return(apply( file.vec.list.df, 1 , paste , collapse = "_" ))
}













# Plot a set of trajectories
#
# @param traj.file.vec A vector of character. The paths to the trajectory files
# @return A ggplot object
.plotTrajs <- function(traj.file.vec){
  long <- NULL; lon <- NULL; lat <- NULL; group <- NULL; map.xlim <- NULL; 
  trajlabel <- NULL; map.ylim <- NULL
  # traj.file.vec <- "/home/lagee/Documents/alber/test/tmp/rba/co/simNoHead/rba_2010_10_27_16_1219.20"
  wgs84 <-  sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  traj.dat.list <- files2df(file.vec = traj.file.vec,                          # read the trajectory files into a list of data.frames 
                             header = FALSE, skip = 0, cnames = HYSPLIT.COLNAMES)
  traj.dat.list <- .listname2data.frame(df.list = traj.dat.list,                # add file name as column
                                        colname = "file.vec")
  traj.dat.df <- do.call("rbind", traj.dat.list)                                # collapse to a single data.frame
  traj.dat.df["profile"] <- .filename2profile(unlist(traj.dat.df["file.vec"]))  # add profile column
  traj.dat.df["trajlabel"] <- .formatTrajname(unlist(traj.dat.df["file.vec"]))  # add a label to order by height in the plot
  trajmap <- ggplot2::ggplot(data = ggplot2::map_data(map = "world"), mapping = ggplot2::aes(long, lat, group = group)) +
    ggplot2::geom_polygon(fill = "white", colour = "black") +                
    ggplot2::coord_quickmap(xlim = map.xlim, ylim = map.ylim, expand = TRUE) + 
    ggplot2::labs(x = "longitude", y = "latitude", color = "trajectory") + 
    ggplot2::geom_path(data = traj.dat.df,                                      # add the trajectories
                       mapping = ggplot2::aes(x = lon, y = lat, 
                                              group = trajlabel, 
                                              colour = trajlabel))
  return(trajmap)
}






#' @title Replace NULLs in list with empty data.frames
#' @name add_na_df
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Replace NULLs in list with empty data.frames. The data.frames in 
#' the list have the same columns and data types.
#'
#' @param list_of_df A list of data.frame.
#' @return           A list.
#' @export
add_na_df <- function(list_of_df){
 
  is_na_vec <- sapply(list_of_df, function(x){
    if (length(x) == 1 && is.na(x))
        return(TRUE)
    if (is.null(x))
        return(TRUE)
    
    return(FALSE)
  })
   
  # take the first non-empy data.frame
  template_df <- list_of_df[[which(!(is_na_vec))[1]]]
  
  # replate the template values with NAs
  template_df <- as.data.frame(lapply(template_df, function(x){NA}))

  # replace the NULL in the input list with the template_df 
  res <- lapply(list_of_df, function(x, template_df){
    if (length(x) == 1 && is.na(x))
      return(template_df)
    if (is.null(x))
      return(template_df)
    return(x)
  }, template_df = template_df)
  
  return(res)
}





# functions used by new scripts

#' @title Read a file and remove its header
#' @name removeHeader
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Read a file and remove its header
#'
#' @param file_path A length-1 character vector. A path to a file
#' @param col_names A character. The column names fo the returned data frame
#' @param skip      A length-1 numeric. Number of lines to skip when reading the file
#' @return          A data.frame
#' @export
removeHeader <- function(file_path, col_names, skip){
  file.dat <- utils::read.table(file = file_path, sep = "", header = FALSE, 
                                skip = skip, stringsAsFactors = FALSE)
  colnames(file.dat) <- col_names
  return(file.dat)
}


#' @title Compute the concentration on the ground
#' @name floorconcentration
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Compute the concentration on the ground
#' 
#' @param dif_obs_bkg  A numeric. The difference between the observed conetration and the background concentration
#' @param hfloor       A numeric. The hieght of the floor
#' @param temp         A numeric. The tempearture of the first observation above the floor
#' @param height       A numeric. The height of the first observaton above the floor
#' @param molair       A numeric. The molarity of the air
#' @return             A numeric. The concentration on the ground
floorconcentration <- function(dif_obs_bkg, hfloor, temp, height, molair){
  ((dif_obs_bkg * exp(-hfloor / 1013.25 / 7) / 0.0000820574587 / (temp + 273 + ((height - hfloor) * 0.0059))) + (molair * dif_obs_bkg))/2
  # ((S16         * EXP(-P$1    / 1013.25 / 7)/  F$2             / (U16  + 273 + ((P16 -    P$1   ) * 0.0059))) + (Y16 *    S16        ))/2
}
