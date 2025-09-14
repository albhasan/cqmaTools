#' Identify outliers
#'
#' @description
#' Identify the outliers in the given vector.
#'
#' @param x a numeric.
#'
#' @return a logical.
#'
#' @export
#'
is_outlier <- function(x) {
  return(x %in% grDevices::boxplot.stats(x)$out)
}
