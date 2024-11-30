# The name of the columns of the raw data file.
BRIEFCASE.COLNAMES <- c("site", "year", "month", "day", "hour", "min", "flask",
                        "V8", "concentration", "flag", "V11", "ayear",
                        "amonth", "aday", "ahour", "amin", "lat", "lon",
                        "height", "eventnumber", "flat", "flon", "fheight")

# Pattern of trajectories' filenames.
TRAJECTORY.FILENAME.PATTERN = "^[a-zA-Z]{3}_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]+[.][0-9]+$"

# Name and type of the metadata embedded in trajectories' file names.
# NOTE: It must match TRAJECTORY.FILENAME.PATTERN.
TRAJECTORY.FILENAME.METADATA <- c(site = "character", year = "integer",
                                  month = "integer", day = "integer",
                                  hour = "integer", height = "double")

# The name of the columns in the Hysplit files.
TRAJECTORY.COLNAMES <-
  c(V1 = "character", V2 = "character", year = "integer", month = "integer",
    day = "integer", hour = "integer", min = "double", V8 = "character",
    V9 = "character", lat = "double", lon = "double", height = "double",
    pressure = "double")

# Number of rows to skip when reading a trajectory file.
TRAJECTORY.SKIP <- 7

YEAR.TRIMESTERS <- c("t1", "t1", "t1", "t2", "t2", "t2",
                     "t3", "t3", "t3", "t4", "t4", "t4")
YEAR.SEMESTERS <- c("s1", "s1", "s1", "s1", "s1", "s1",
                    "s2", "s2", "s2", "s2", "s2", "s2")
YEAR.YEAR      <- c("year", "year", "year", "year", "year", "year",
                    "year", "year", "year", "year", "year", "year")
