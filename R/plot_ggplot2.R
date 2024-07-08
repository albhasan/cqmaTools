#' Plot a set of trajectories
#'
#' @description
#' Plot the trajectories in the input files.
#'
#' @param files a character. Paths to trajectory files.
#' @param cnames a character. Names for the columns in the given files.
#' @param header a logical(1). Do files have headers?
#' @param skip an integer(1). Number of lines to skip from each file.
#' @param xlim,ylim a numeric(2). Longitude and latitude ranges of the 
#'   resulting plot (map).
#'
#' @return A ggplot object
#'
plot_trajectories <- function(files, cnames = HYSPLIT.COLNAMES,
                              header = FALSE, skip = 7,
                              xlim = c(-80, -30), ylim = c(-40, 10)) {

    group <- lat <- lon <- long <- trajlabel <- NULL

    stopifnot("File(s) not found!" = vapply(files, file.exists, logical(1)))

    files_df_ls <- files2df(
        files = files, 
        header = header, 
        skip = skip, 
        cnames = cnames
    )

    data_df_ls  <- listnames2dataframes(df_ls = files_df_ls,
        cname = "filename")

    traj_df <- do.call("rbind", data_df_ls)
    traj_df["profile"] <- filename2profile(traj_df[["filename"]])
    traj_df["trajlabel"] <- format_traj_names(traj_df[["filename"]])

    trajmap <- 
        ggplot2::ggplot(data = ggplot2::map_data(map = "world"),
                        mapping = ggplot2::aes(long, lat, group = group)) +
        ggplot2::geom_polygon(fill = "white", colour = "black") +
        ggplot2::coord_quickmap(xlim = xlim, ylim = ylim, expand = TRUE) +
        ggplot2::labs(x = "longitude", y = "latitude", color = "trajectory") +
        ggplot2::geom_path(
            data = traj_df,
            mapping = ggplot2::aes(x = lon, y = lat,
                                   group = trajlabel, 
                                   colour = trajlabel)
        )

    return(trajmap)
}

