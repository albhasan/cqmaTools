#' The name of the columns in the file resulting from processing briefcases'
#' metadata.
#'
#' @details the column names and their meaning.
#' * sample Sample number.
#' * plan,planmts Flight-plan height in feet and meters.
#' * start,startmts Height at the begining of the filght in feet and meters.
#' * end,endmts Height at the end of the fligth in feet and meters.
#' * min,minmts Mininum height during the flight in feet and meters.
#' * max,maxmts Maxinum height during the flight in feet and meters.
#' * mean,meanmts Mean flight height in feet and meters.
#' * temperature (C) Air temperature in celsius.
#' * humidity (%RH) Percentage of relative humidity.
#' * pressure Pressure in milibars.
#' * profile Profile code.
#' @md
#'
#' @export
#'
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
#'
#' @details the column names and their meaning.
#' * site Name of the sample site.
#' * year Year of sample acquisition.
#' * month Month of sample acquisition.
#' * day Day of sample acquisition.
#' * hour Hour of sample acquisition.
#' * min Minute of sample acquisition.
#' * flask Sample's flask code.
#' * V8 Unknown.
#' * concentration Gas concentration.
#' * flag Observation status. See \code{RAWDATA.VALID.FLAGS}.
#' * wmo_code Equipment code provided by the World Metereological Organization.
#' * ayear Year of sample analysis.
#' * amonth Month of sample analysis.
#' * aday Day of sample analysis.
#' * amin Minute of sample analysis.
#' * lat Sample latitude.
#' * lon Sample longitude.
#' * height Sample height.
#' * eventnumber Unknown.
#' * flat Unknown.
#' * flon Unknown.
#' * fheight Unknown.
#' @md
#'
#' @export
#'
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
  wmo_code = "character",
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
#' * traj_number Trajectory number.
#' * grid_number Metereological grid number or antecedent trajectory
#' number.
#' * year Year of the point.
#' * month Month of the point.
#' * day Day of the point.
#' * hour Hour of the point.
#' * minute Minute of the point.
#' * forecast Forecast hour at point
#' * traj_age Age of the trajectory in hours.
#' * latitude Position latitude.
#' * longitude Position longitude.
#' * height Position height in meters above ground.
#' * pressure n diagnostic output variables; 1st to be output is
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
#' @details the column names and their meaning.
#' * traj_number Trajectory number.
#' * grid_number Metereological grid number or antecedent trajectory
#' number.
#' * site Name of the sample site.
#' * year Year of the point.
#' * month Month of the point.
#' * day Day of the point.
#' * hour Hour of the point.
#' @md
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
#' The name of the columns in the trajectory files produced by the Hysplit software.
#' @export
HYSPLIT.COLNAMES <- c("V1", "V2", "year", "month", "day", "hour", "min",
                      "V8", "V9", "lat", "lon", "height", "pressure")
