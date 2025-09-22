#TODO: Document each column.

#' The name of the columns in the file resulting from processing briefcases' metadata.
#' @export
BRIEFCASE.COLNAMES <- c(
  "sample" = "integer",
  "plan" = "integer", # Flight plan height
  "start" = "integer",
  "end" = "integer",
  "min" = "integer",
  "max" = "integer",
  "mean" = "integer",
  "temperature (C)" = "numeric",
  "humidity (%RH)" = "numeric",
  "pressure (mbar)" = "numeric",
  "planmts" = "numeric",
  "startmts" = "numeric",
  "endmts" = "numeric",
  "minmts" = "numeric",
  "maxmts" = "numeric",
  "meanmts" = "numeric",
  "profile" = "character"
)

#' Number of lines to skip from a briefcase metadata file.
#' @export
BRIEFCASE.SKIP <- 0

#' The name of the columns in the file resulting from analyzing gas samples.
#' @export
RAWDATA.COLNAMES <- c(
  site = "character",
  year = "integer", # Acquisition.
  month = "integer",
  day = "integer",
  hour = "integer",
  min = "integer",
  flask = "character",
  V8 = "character",
  concentration = "double",
  flag = "character",
  wmo_code = "character", # Codigo utilizado pela WMO Equipamento usado para analisy (crhomatografo, analizador de CO.
  ayear = "integer", # Dia do analyse
  amonth = "integer",
  aday = "integer",
  ahour = "integer",
  amin = "integer",
  lat = "double",
  lon = "double",
  height = "double",
  eventnumber = "character",
  flat = "double",
  flon = "double",
  fheight = "double"
)

#' Number of lines to skip from a rawdata file.
#' @export
RAWDATA.SKIP <- 0

#' Flags for identifying valid observations in raw data. 
#' @export
RAWDATA.VALID.FLAGS <- c("...", "..>", "..<")


#' Pattern of trajectories' filenames.
#'
#' @description Pattern used for matching file names corresponding to
#' trajectory files. This is closely relate to  TRAJECTORY.METADATA.
#'
#' @export
#'
TRAJECTORY.FILENAME.PATTERN <-
  "^[a-zA-Z]{3}_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]+[.][0-9]+$"

#' Names of the columns in the files with trajectory data.
#'
#' These names and descriptions were taken from the Hysplit users' guide (April
#' 2025); the End Point file format, record loop 6 through the number of hours
#' in the simulation.
#'
#' @details the column names and their meaning.
#' * parameter traj_number Trajectory number.
#' * parameter grid_number Metereological grid number or antecedent trajectory
#' number.
#' * parameter year Year of the point.
#' * parameter month Month of the point.
#' * parameter day Day of the point.
#' * parameter hour Hour of the point.
#' * parameter minute Minute of the point.
#' * parameter forecast Forecast hour at point
#' * parameter traj_age Age of the trajectory in hours.
#' * parameter latitude Position latitude.
#' * parameter longitude Position longitude.
#' * parameter height Position height in meters above ground.
#' * parameter pressure n diagnostic output variables; 1st to be output is
#' always pressure.
#' @md
#'
#' @export
#'
TRAJECTORY.COLNAMES <- c(
  traj_number = "character",
  grid_number = "character",
  year = "integer",
  month = "integer",
  day = "integer",
  hour = "integer",
  minute = "integer",
  forecast = "character",
  traj_age = "double",
  latitude = "double",
  longitude = "double",
  height = "double",
  pressure = "double"
)

#' Metadata contained in trajectories' file names.
#'
#' @description
#' Description of metadata in the names of the trajectories' files. This is
#' closely related to TRAJECTORY.FILENAME.PATTERN.
#'
#' @export
#'
TRAJECTORY.METADATA <- c(
  site = "character",
  year = "integer",
  month = "integer",
  day = "integer",
  hour = "integer",
  height = "double"
)

#' Number of lines to skip from a trajectory file.
#' @export
TRAJECTORY.SKIP <- 7

#' Months grouped into trimesters.
#' @export
YEAR.TRIMESTERS <- c("t1", "t1", "t1", "t2", "t2", "t2",
                     "t3", "t3", "t3", "t4", "t4", "t4")

#' Months grouped into semesters.
#' @export
YEAR.SEMESTERS <- c("s1", "s1", "s1", "s1", "s1", "s1",
                    "s2", "s2", "s2", "s2", "s2", "s2")

#' Months grouped into a year.
#' @export
YEAR.YEAR      <- c("year", "year", "year", "year", "year", "year",
                    "year", "year", "year", "year", "year", "year")

#TODO: Deprecated!
#TODO: Document each column.
#' The name of the columns in the trajectory files produced by the Hysplit software.
#' @export
HYSPLIT.COLNAMES <- c("V1", "V2", "year", "month", "day", "hour", "min",
                      "V8", "V9", "lat", "lon", "height", "pressure")
