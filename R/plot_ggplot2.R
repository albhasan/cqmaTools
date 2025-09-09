#' Build a map trajectories
#'
#' @description
#' Build a map of backtrajectories.
#'
#' @param data_tb A tibble with backtrajectory data.
#' @param cid A character(1). Name of the column that identifies each
#' backtrajectory
#' @param clon,clat A character(1). Names of the columns with the longitude and
#' latitude.
#' @param range_lon,range_lat a double(2). Minimum and maximum longitude and
#' latitude of the map.
#'
#' @return
#' A ggplot object.
#'
#' @export
#'
get_map_plot <- function(data_tb, cid, clon, clat,
                         range_lon = NA, range_lat = NA) {

  .data <- long <- lat <- group <- NULL

  # Estimate coordinate ranges from backtrajectories.
  if (any(is.na(range_lon))) {
    range_lon <- range(data_tb[[clon]])
  }
  if (any(is.na(range_lat))) {
    range_lat <- range(data_tb[[clat]])
  }

  map_plot <-
    ggplot2::ggplot(
      data = ggplot2::map_data(map = "world"),
      mapping = ggplot2::aes(x = long, y = lat, group = group)
    ) +
    ggplot2::geom_polygon(fill = "white", colour = "black") +
    ggplot2::coord_quickmap(xlim = range_lon, ylim = range_lat, expand = TRUE) +
    ggplot2::labs(x = "longitude", y = "latitude", color = "") +
    ggplot2::geom_path(
      data = data_tb,
      mapping = ggplot2::aes(
        x = .data[[clon]],
        y = .data[[clat]],
        colour = .data[[cid]],
        group = .data[[cid]]
      )
    )

  return(map_plot)
}



#' Build a profile plot from the given trajectories
#'
#' @description
#' Build a plot object by grouping the given trajectories into profiles.
#'
#' @param data_tb A tibble with backtrajectory data.
#' @param cprofileid A character(1). Name of the column that identifies each
#' backtrajectory.
#' @param cghg A character(1). Name of the column with GHG concentrations.
#' @param cheight A character(1). Name of the column with sample heights.
#' @param add_mean_line A logical(1). Should be added a line representing the
#' profile's mean?
#' @param mean_line_color A character(1). Name of the color for the profile's
#' mean line.
#' @param mean_line_width A numeric(1). Width of the mean line.
#' @param add_sd_shadow A logical(1). Shoulb be added a shadow showing the
#' profile's standard deviation?
#' @param xlab,ylab A character(1). Label of the axes.
#' @param title A character(1). Plot title.
#'
#' @return
#' A `ggplot2` object.
#'
#' @export
#'
get_profile_plot <- function(data_tb, cprofileid, cghg, cheight,
                             add_mean_line = TRUE, mean_line_color = "black",
                             mean_line_width = 2, add_sd_shadow = TRUE,
                             xlab = "Height", ylab = "GHG concentration",
                             title = NA) {

  .data <- .ghg_mean <- .ghg_sd <- NULL

  stopifnot("Field not found!" = all(c(cprofileid, cghg, cheight) %in%
                                       colnames(data_tb)))

  data_tb <-
    data_tb |>
    dplyr::arrange(.data[[cprofileid]], .data[[cheight]])

  profile_plot <-
    ggplot2::ggplot(
      data = data_tb,
      mapping = ggplot2::aes(
        x = .data[[cheight]],
        y = .data[[cghg]]
      )
    )

  if (add_sd_shadow || add_mean_line) {
    stats_df <-
      data_tb |>
      dplyr::summarise(
        .ghg_sd = stats::sd(.data[[cghg]], na.rm = TRUE),
        .ghg_mean = mean(.data[[cghg]], na.rm = TRUE),
        .by = tidyselect::all_of(cheight)
      ) |>
      dplyr::mutate(
        .ribbon_min = .ghg_mean - .ghg_sd,
        .ribbon_max = .ghg_mean + .ghg_sd
      ) |>
      dplyr::arrange(.data[[cheight]])
  }

  if (add_sd_shadow) {
    profile_plot <-
      profile_plot +
      ggplot2::geom_ribbon(
        data = stats_df,
        mapping = ggplot2::aes(
          x = .data[[cheight]],
          y = .data[[".ghg_mean"]],
          ymin = .data[[".ribbon_min"]],
          ymax = .data[[".ribbon_max"]]
        ),
        alpha = 0.15
      )
  }

  profile_plot <-
    profile_plot +
    ggplot2::geom_path(
      mapping = ggplot2::aes(
        group  = .data[[cprofileid]],
        colour = .data[[cprofileid]]
      )
    ) +
    ggplot2::geom_point(mapping = ggplot2::aes(colour = .data[[cprofileid]])) +
    ggplot2::coord_flip()

  if (add_mean_line) {
    profile_plot <-
      profile_plot +
      ggplot2::geom_path(
        data = stats_df,
        mapping = ggplot2::aes(
          x = .data[[cheight]],
          y = .data[[".ghg_mean"]],
        ),
        color = mean_line_color,
        linewidth = mean_line_width
      )
  }

  profile_plot <-
    profile_plot +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  if (!is.na(title)) {
    profile_plot <-
      profile_plot +
      ggplot2::ggtitle(title)
  }

  return(profile_plot)
}
