#' Build a profile name from trajectory paths
#'
#' @description
#' Build a profile name using the names of the given files. The profile names
#' are made of the first 4 elements of the trajectories' filenames: site, year,
#' month, and day.
#'
#' @param files a character. Path to trajectory files.
#' @param traj_pat a character. Regular expression pattern that the 
#' trajectories' filenames must match.
#'
#' @return a character.
#'
filename2profile <- function(files,
                         traj_pat = "^[A-Z]{3}_[0-9]{4}_[0-9]{2}_[0-9]{2}.") {

    stopifnot("Some files don't match the given pattern" =
        all(seq(files) %in% grep(x = basename(files), pattern = traj_pat))
    )

    bn <- basename(files)

    # Format function.
    f <- function(x) {
        toupper(paste(unlist(strsplit(x, split = "_"))[1:4], collapse = "_"))
    }

    return(vapply(bn, f, character(1)))

}

