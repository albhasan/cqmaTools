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

#' Get metadata from trajectory files
#'
#' @description
#' Build a data frame with metadata extracted from the given filenames.
#'
#' @param files a character. Path to trajectory files.
#' @param cnames a named character. The vector's names are the names for the 
#'   resulting data frame and its values are their data types.
#' @param m_period a vector with 12 elements identifying a period for each
#'   month (e.g. trimester, semester, etc.).
#' 
#' @return a data frame.
#'
#' @export
#'
get_trajectory_metadata <- function(files, cnames = TRAJECTORY.COLNAMES,
                                    m_period = YEAR.TRIMESTERS) {

    stopifnot("Invalid column type!" = cnames %in% 
        c("character", "double", "integer"))
    stopifnot("`month` column not found!" = "month" %in% names(cnames))
    stopifnot("`m_period` length must be 12!" = length(m_period) == 12)

    files_df <- data.frame(do.call(
        what = rbind,
        strsplit(basename(files), split = "_")
    ))
    files_df <- cast_df_cols(files_df, cnames = cnames)

    # Add the month periods.
    if ("month" %in% colnames(files_df))
        files_df["m_period"] <- m_period[files_df[["month"]]]

    # Add path to files.
    files_df["filepath"] <- files

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
#' Filter the trajectories in the given list of data frame and bind them into
#' a single data frame.
#'
#' @param traj Either a data frame or a list of them. Each data frame contains
#'   data of a single trajectory.
#' @param from_row,to_row a numeric(1). Use a subset of rows from each data
#'   frame.
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
                        vert_min_lon    = -Inf, vert_max_lon    = Inf,
                        vert_min_lat    = -Inf, vert_max_lat    = Inf,
                        vert_min_height = -Inf, vert_max_height = Inf,
                        min_per_vert_in_hrange = 0) {

    if (is.data.frame(traj)) {

        # Filter number of rows in data frame.
        if (!all(from_row == 1, to_row == Inf)) {
            traj <- traj[from_row:to_row, ]
        }

        # Filter trajectories by their vertex percentage in height range.
        if (min_per_vert_in_hrange > 0) {
            # Compute percentage of vertices in the range from traj_min_height
            # to traj_max_height.
            per_vert_in_range <- sum(traj[[cheight]] >= traj_min_height &
                                     traj[[cheight]] <= traj_max_height) / 
                                 nrow(traj)
            if(per_vert_in_range < min_per_vert_in_hrange)
                return(traj[rep(FALSE, times = nrow(traj)),])
        }

        # Filter data frames (trajectories) by height, longitude, and latitude.
        if (!all(traj_min_height == -Inf, traj_max_height == Inf)) {
            traj <- filter_data_frames(x = traj,
                cname = cheight,
                min = traj_min_height,
                max = traj_max_height)
            if (any(all(is.na(traj)), nrow(traj) == 0)) {
                warning("No trajectory meets the height filter!")
                return(traj)
            }
        }

        if (!all(traj_min_lon == -Inf, traj_max_lon == Inf)) {
            traj <- filter_data_frames(x = traj,
                cname = clon,
                min = traj_min_lon,
                max = traj_max_lon)
            if (any(all(is.na(traj)), nrow(traj) == 0)) {
                warning("No trajectory meets the longitude filter!")
                return(traj)
            }
        }

        if (!all(traj_min_lat == -Inf, traj_max_lat == Inf)) {
            traj <- filter_data_frames(x = traj,
                cname = clat,
                min = traj_min_lat,
                max = traj_max_lat)
            if (any(all(is.na(traj)), nrow(traj) == 0)) {
                warning("No trajectory meets the latitude filter!")
                return(traj)
            }
        }

        # Filter trajectories' vertices by height, longitude, and latitude.
        if (!all(vert_min_height == -Inf, vert_max_height == Inf))
        traj<- traj[traj[[cheight]] >= vert_min_height &
        traj[[cheight]] <= vert_max_height,]
        if (nrow(traj) == 0) {
            warning("No trajectory vertex meets the height filter!")
            return(traj)
        }

        if (!all(vert_min_lon == -Inf, vert_max_lon == Inf))
        traj<- traj[traj[[clon]] >= vert_min_lon &
        traj[[clon]] <= vert_max_lon,]
        if (nrow(traj) == 0) {
            warning("No trajectory vertex meets the longitude filter!")
            return(traj)
        }

        if (!all(vert_min_lat == -Inf, vert_max_lat == Inf)) {
            traj<- traj[traj[[clat]] > vert_min_lat &
                        traj[[clat]] < vert_max_lat,]
        }

        if (nrow(traj) == 0) {
            warning("No trajectory vertex meets the longitude filter!")
            return(traj)
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
            traj_max_lat = traj_max_lat,
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

