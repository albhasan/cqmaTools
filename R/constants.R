# The name of the columns in the Hysplit files.
HYSPLIT.COLNAMES <-
  c("V1", "V2", "year", "month", "day", "hour", "min", "V8", "V9", 
    "lat", "lon", "height", "pressure")

# The name of the columns of the raw data file.
RAW.DATA.COLNAMES <- c("site", "year", "month", "day", "hour", "min", "flask",
                       "V8", "concentration", "flag", "V11", "ayear", "amonth",
                       "aday", "ahour", "amin", "lat", "lon", "height",
                       "eventnumber", "flat", "flon", "fheight")

# Pattern of trajectories' filenames.
TRAJECTORY.FILENAME.PATTERN = "^[a-zA-Z]{3}_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]+[.][0-9]+$"

# Trajectory's column names and type.
TRAJECTORY.COLNAMES = c(site = "character", year = "integer",
                        month = "integer", day = "integer", hour = "integer",
                        height = "double")

YEAR.TRIMESTERS <- c("t1", "t1", "t1", "t2", "t2", "t2",
                     "t3", "t3", "t3", "t4", "t4", "t4")
YEAR.SEMESTERS <- c("s1", "s1", "s1", "s1", "s1", "s1",
                    "s2", "s2", "s2", "s2", "s2", "s2")
YEAR.YEAR      <- c("year", "year", "year", "year", "year", "year",
                    "year", "year", "year", "year", "year", "year")
