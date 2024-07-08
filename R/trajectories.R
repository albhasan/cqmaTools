#' @title Intersect trajectories
#' @name intersect_trajectories
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Identify trajectories' segments that cross a limit.
#
#' @param traj_ls A list of data frames. Eacha data frame represents a
#' trajectory.
#' @param limit An sf object. A limit.
#' @param crs A numeric. The EPSG code of the trajectories.
#' @param row_after A logical. Should we return the id of the row after the 
#' limit? If no, return the row id before the limit.
#'
#' @return A numeric indicating a row id for each trajectory in the input list.
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
#' List the trajectory files in the given directory and build a data frame with
#' the metadata stores in their names.
#'
#' @param path a character(1). Path to a directory.
#' @param cnames a named character vector. The vector's names are the names for 
#'   the resulting data frame and its values are their data types.
#' @param trimester a vector with 12 elements identifying the trimester of each
#'   month.
#' @param pattern a character(1). Pattern to recognize trajectory files.
#' 
#' @return a data frame.
#'
#' @export
#'
get_trajectory_metadata <- function(path, 
                                    cnames = c(site = "chr", year = "int", 
                                               month = "int", day = "int", 
                                               hour = "int", height = "dbl"),
                                    trimester = c("t1", "t1", "t1", 
                                                  "t2", "t2", "t2", 
                                                  "t3", "t3", "t3", 
                                                  "t4", "t4", "t4"),
pattern = "^[a-zA-Z]{3}.[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]+.[0-9].$") {

    stopifnot("Invalid column type!" = cnames %in% c("chr", "int", "dbl"))
    stopifnot("Trimester length must be 12!" = length(trimester) == 12)

    # Get a data frame of file names.
    files <- list.files(
        path = path, 
        pattern = pattern,
        full.names = TRUE,
        recursive = TRUE
    )
    files_df <- data.frame(do.call(what = rbind, 
                                   strsplit(basename(files), split = "_")))

    # Cast data frame columns.
    colnames(files_df) <- names(cnames)
    for (cn in names(cnames)) {
        f <- identity
        if (cnames[cn] == "int") f <- as.integer
        if (cnames[cn] == "dbl") f <- as.double
        files_df[cn] <- f(files_df[[cn]])
    }

    # Add the trimesters.
    if ("month" %in% colnames(files_df))
        files_df["trimester"] <- trimester[files_df[["month"]]]

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
pattern = "^[A-Z]{3}_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]{2}_[0-9]+[.][0-9]+") {

    stopifnot("Invalid trajectory names found!" = 
        all(seq(traj_names) %in% grep(x = traj_names, pattern = pattern))
    )

    names_ls <- strsplit(traj_names, split = "_")
    names_df <- as.data.frame(do.call("rbind", names_ls))

    names_df["V6"] <- as.numeric(names_df[["V6"]])
    names_df["V6"] <- formatC(names_df[["V6"]], digits = 1, width = 6,
        format = "f", flag = "0")
    names_df <- names_df[, c("V1", "V2", "V3", "V4", "V6", "V5")]

    return(apply(names_df, 1 , paste , collapse = "_" ))

}



