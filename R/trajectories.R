#' @title Intersect trajectories
#' @name intersect_trajectories
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Identify trajectories' segments that cross a limit.
#
#' @param traj_ls a list of data frames. Eacha data frame represents a
#' trajectory.
#' @param limit an sf object (line). A limit.
#' @param crs a numeric. The EPSG code of the trajectories.
#' @param row_after a logical. Should we return the id of the row after the 
#' limit? If no, return the row id before the limit.
#'
#' @return a numeric indicating a row id for each trajectory in the input list.
#'
#' @export
#'
intersect_trajectories <- function(traj_ls, limit, crs = 4326,
                                   row_after = TRUE){

    # Create a line for each pair of vertices in each trajectory.
    traj_ls <- lapply(traj_ls, traj2lines)

    # Intersect the trajectories' lines with the limit.
    traj_lim_in <- lapply(traj_ls, function(x, y){
        sapply(x, sf::st_intersects, y = y)
    }, y = limit)
    traj_lim_in <- lapply(traj_lim_in, function(x) {
        sapply(x, function(y) {length(y) > 0})
    })

    # Find the first vertex of the line that intersects the limit.
    row_id <- sapply(traj_lim_in, base::Position, f = isTRUE, nomatch = 0)

    if (row_after)
        row_id <- ifelse(row_id > 0, row_id + 1, 0)

    return(row_id)

}



#' @title Build trajectory lines
#' @name traj2lines
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Build a line (st_linestring) for each pair of points in the
#' given trajectory.
#'
#' @param traj_df A data.frame with trajectory data in HYSPLIT format.
#' @param clon a character(1). The name of the longitude (x) column.
#' @param clat a character(1). The name of the latitude (x) column.
#'
#' @return         A list of [sf::st_linestring] objects.
#'
traj2lines <- function(traj_df, clon = "lon", clat = "lat"){
    stopifnot("Expected data frame!" = is.data.frame(traj_df))
    lines_ls <- list()
    for (i in seq(nrow(traj_df))) {
        if (i == 1) next
        lines_ls[[i - 1]] <-
            sf::st_linestring(
                as.matrix(traj_df[(i - 1):i, c(clon, clat)]),
                dim = "XY"
            )
    }
    return(lines_ls)
}

#' Get metadata from trajectory file names
#'
#' @description
#' Build a data frame with metadata extracted from the given filenames.
#'
#' @param files a character. Path to trajectory files.
#' @param cnames a character. Column names for the resulting data frame.
#' @param ctypes a character. Column data types for the resulting data frame.
#'
#' @return a data frame.
#'
#' @export
#'
get_trajectory_metadata <- function(
  files,
  cnames = names(TRAJECTORY.FILENAME.METADATA),
  ctypes = TRAJECTORY.FILENAME.METADATA
) {

  stopifnot("Inconsistent column names and types" =
              length(cnames) == length(ctypes))
  stopifnot("Invalid column type!" = ctypes %in%
              c("character", "double", "integer"))

  files_df <- data.frame(do.call(
    what = rbind,
    strsplit(basename(files), split = "_")
  ))
  files_df <- cast_df_cols(files_df, ctypes = ctypes)
  colnames(files_df) <- cnames

  # Add path to files.
  files_df["file_path"] <- files

  return(files_df)

}



#' Format trajectories' names
#'
#' @description
#' Put the height before the hour in given trajectories' names. Also, ensure
#' the height has the same number of digits.
#'
#' @param traj_names a character. Names of trajectories.
#' @param pattern a character(1). Pattern of valid trajectory names.
#'
#' @return a character.
#'
format_traj_names <- function(traj_names,
                              pattern = TRAJECTORY.FILENAME.PATTERN) {

    stopifnot("Invalid trajectory names found!" = 
        all(seq(traj_names) %in% grep(x = traj_names, pattern = pattern))
    )

    names_ls <- strsplit(basename(traj_names), split = "_")
    names_df <- as.data.frame(do.call("rbind", names_ls))

    names_df["V6"] <- as.numeric(names_df[["V6"]])
    names_df["V6"] <- formatC(names_df[["V6"]], digits = 1, width = 6,
        format = "f", flag = "0")
    names_df <- names_df[, c("V1", "V2", "V3", "V4", "V6", "V5")]

    return(apply(names_df, 1 , paste , collapse = "_" ))

}



#' Filter trajectories
#'
#' @description
#' Filter whole trajectories from the given list of data frames.
#'
#' @param traj Either a data frame or a list of them. Each data frame contains
#'   data of a single trajectory.
#' @param clon,clat,cheight a character(1). Names of the longitude, latitude,
#'   and height columns in the given data frame.
#' @param traj_min_lon,traj_max_lon,traj_min_lat,traj_max_lat,traj_min_height,traj_max_height 
#'   a numeric(1). Remove trajectories which, at some vertex, fall outside of
#'   these ranges.
#' @param vert_min_lon,vert_max_lon,vert_min_lat,vert_max_lat,vert_min_height,vert_max_height
#'   a numeric(1). Remove vertices from trajectories falling outside of these
#'   ranges.
#' @param min_per_vert_in_hrange a double(1). Minimum percentage of vertices
#'   inside height range for a trajectory to be valid.
#'
#' @return Either a data frame or a list ot them.
#'
filter_traj <- function(traj,
                        from_row = 1, to_row = Inf,
                        clon = "lon", clat = "lat", cheight = "height",
                        traj_min_lon    = -Inf, traj_max_lon    = Inf,
                        traj_min_lat    = -Inf, traj_max_lat    = Inf,
                        traj_min_height = -Inf, traj_max_height = Inf,
                        min_per_vert_in_hrange = 0) {

  if (is.data.frame(traj)) {

    # Filter trajectories by their vertex percentage in height range.
    if (min_per_vert_in_hrange > 0) {
      # Compute percentage of vertices in the range from traj_min_height
      # to traj_max_height.
      per_vert_in_range <-
        sum(
          traj[[cheight]] >= traj_min_height &
            traj[[cheight]] <= traj_max_height
        ) / nrow(traj)
      if (per_vert_in_range < min_per_vert_in_hrange)
        return(traj[rep(FALSE, times = nrow(traj)),])
    }
    # Filter data frames (trajectories) by height.
    if (!all(traj_min_height == -Inf, traj_max_height == Inf)) {
      traj <- filter_data_frames(
        x = traj,
        cname = cheight,
        min = traj_min_height,
        max = traj_max_height
      )
      if (any(all(is.na(traj)), nrow(traj) == 0)) {
        warning("No trajectory meets the height filter!")
        return(traj)
      }
    }
    # Filter data frames (trajectories) by longitude.
    if (!all(traj_min_lon == -Inf, traj_max_lon == Inf)) {
      traj <- filter_data_frames(
        x = traj,
        cname = clon,
        min = traj_min_lon,
        max = traj_max_lon
      )
      if (any(all(is.na(traj)), nrow(traj) == 0)) {
        warning("No trajectory meets the longitude filter!")
        return(traj)
      }
    }
    # Filter data frames (trajectories) by latitude.
    if (!all(traj_min_lat == -Inf, traj_max_lat == Inf)) {
      traj <- filter_data_frames(
        x = traj,
        cname = clat,
        min = traj_min_lat,
        max = traj_max_lat
      )
      if (any(all(is.na(traj)), nrow(traj) == 0)) {
        warning("No trajectory meets the latitude filter!")
        return(traj)
      }
    }

    return(traj)

  } else if (is.list(traj)) {
    return(lapply(traj, filter_traj,
      from_row = from_row, to_row = to_row,
      min_per_vert_in_hrange = min_per_vert_in_hrange,
      clon = clon, clat = clat, cheight = cheight,
      traj_min_height = traj_min_height,
      traj_max_height = traj_max_height,
      traj_min_lon = traj_min_lon,
      traj_max_lon = traj_max_lon,
      traj_min_lat = traj_min_lat,
      traj_max_lat = traj_max_lat
    ))
  } else {
    stop("Unknown object type!")
  }

}



#' Filter trajectories' vertices
#'
#' @description
#' Filter the trajectories' vertices in the given data frame or list of data
#' frames.
#'
#' @param traj Either a data frame or a list of them. Each data frame contains
#'   data of a single trajectory.
#' @param from_row,to_row a numeric(1). Use a subset of rows from each data
#'   frame.
#' @param clon,clat,cheight a character(1). Names of the longitude, latitude,
#'   and height columns in the given data frame.
#' @param from_row,to_row a numeric(1). Use a subset of rows from each data
#'   frame.
#' @param traj_min_lon,traj_max_lon,traj_min_lat,traj_max_lat,traj_min_height,traj_max_height
#'   a numeric(1). Remove trajectories which, at some vertex, fall outside of
#'   these ranges.
#' @param vert_min_lon,vert_max_lon,vert_min_lat,vert_max_lat,vert_min_height,vert_max_height
#'   a numeric(1). Remove vertices from trajectories falling outside of these
#'   ranges.
#' @param min_per_vert_in_hrange a double(1). Minimum percentage of vertices
#'   inside height range for a trajectory to be valid.
#'
#' @return Either a data frame or a list ot them.
#'
filter_traj_vertices <- function(
  traj,
  clon = "lon", clat = "lat", cheight = "height",
  from_row = 1, to_row = Inf,
  vert_min_lon    = -Inf, vert_max_lon    = Inf,
  vert_min_lat    = -Inf, vert_max_lat    = Inf,
  vert_min_height = -Inf, vert_max_height = Inf
) {

  if (is.data.frame(traj)) {
    # Filter number of rows in data frame.
    if (!all(from_row == 1, to_row == Inf)) {
      traj <- traj[from_row:to_row, ]
    }
    # Filter trajectories' vertices by height.
    if (!all(vert_min_height == -Inf, vert_max_height == Inf))
      traj <- traj[traj[[cheight]] >= vert_min_height &
                     traj[[cheight]] <= vert_max_height, ]
    if (nrow(traj) == 0) {
      warning("No trajectory vertex meets the height filter!")
      return(traj)
    }
    # Filter trajectories' vertices by longitude.
    if (!all(vert_min_lon == -Inf, vert_max_lon == Inf))
      traj <- traj[traj[[clon]] >= vert_min_lon &
                     traj[[clon]] <= vert_max_lon, ]
    if (nrow(traj) == 0) {
      warning("No trajectory vertex meets the longitude filter!")
      return(traj)
    }
    # Filter trajectories' vertices by latitude.
    if (!all(vert_min_lat == -Inf, vert_max_lat == Inf))
      traj <- traj[traj[[clat]] > vert_min_lat &
                     traj[[clat]] < vert_max_lat, ]
    if (nrow(traj) == 0) {
      warning("No trajectory vertex meets the longitude filter!")
      return(traj)
    }

    return(traj)

  } else if (is.list(traj)) {
    return(lapply(traj, filter_traj_vertices,
      clon = clon, clat = clat, cheight = cheight,
      vert_min_height = vert_min_height,
      vert_max_height = vert_max_height,
      vert_min_lon = vert_min_lon,
      vert_max_lon = vert_max_lon,
      vert_min_lat = vert_min_lat,
      vert_max_lat = vert_max_lat
    ))
  } else {
    stop("Unknown object type!")
  }

}



#' Read trajectory data from a file
#'
#' @description
#' Read a file with data from a backtrajectory.
#'
#' @param file_path A character. Path to a data file.
#' @param cnames A character. Column names.
#' @param ctypes A character. Column types.
#'
#' @return a data frame.
#'
#' @export
#'
read_trajectory_file <- function(file_path,
                                 cnames = names(TRAJECTORY.COLNAMES),
                                 ctypes = TRAJECTORY.COLNAMES,
                                 skip = TRAJECTORY.SKIP) {

  data_df <- utils::read.table(
    file = file_path,
    sep = "",
    header = FALSE,
    skip = skip,
    col.names = cnames,
    colClasses = ctypes
  )

  if (all(data_df[["year"]] < 100))
    data_df["year"] <- data_df[["year"]] + 2000

  data_df["date"] <-
    paste(
      data_df[["year"]],
      sprintf("%02d", data_df[["month"]]),
      sprintf("%02d", data_df[["day"]]),
      sep = "-"
    )
  data_df["date"] <-
    paste0(
      data_df[["date"]],
      " ",
      sprintf("%02d", data_df[["hour"]]),
      ":",
      sprintf("%02d", data_df[["min"]])
    )
  data_df["date"] <- as.POSIXct(data_df[["date"]])

  data_df[["year"]] <- data_df[["month"]] <- data_df[["day"]] <- NULL
  data_df[["hour"]] <- data_df[["min"]] <- NULL

  return(data_df)
}



#' Get the trajectories' extent
#'
#' @description
#' Get the extent of the given trajectories.
#'
#' @param .data a data frame of a list of data frame.
#' @param ext_cnames a characcter. Column names representing varibles on which
#' the extent is computed.
#'
#' @return a data frame or a list of data frames.
#'
#' @export
#'
get_traj_extent <-
  function(
    .data,
    ext_cnames = c("lat", "lon", "height", "date")
  ) {
    if (is.data.frame(.data)) {
      stopifnot("Expected columns not found!" =
                  all(ext_cnames %in% colnames(.data)))
      return(get_min_max(.data[ext_cnames]))
    } else if (is.list(.data)) {
      return(.data, get_traj_extent)
    } else {
      stop("Invalid object type!")
    }
  }
