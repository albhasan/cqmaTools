#' Apply a function to a vector using a moving window
#'
#' @description
#' Apply the given function to the given numeric vector using a moving window.
#' The window size twice (plus one) the given hsize.
#'
#' @param x a numeric vector.
#' @param f a function that receives a numeric and returns a numeric(1).
#' @param hsize an integer(1). Number of elements to take before and after
#' to build the moving window.
#' @param ... other arguments passed to f.
#'
#' @return A numeric the same size as the input vector.
#'
#' @export
#'
moving_window <- function(x, f, hsize, ...) {
  res <- vapply(
    X = seq_along(x),
    FUN = function(pos, x, hsize) {
      vec <- x[c(max(1, pos - hsize):min(length(x), pos + hsize))]
      return(f(vec, ...))
    },
    FUN.VALUE = numeric(1),
    x = x,
    hsize = hsize
  )
  return(res)
}


#' Interpolate using splines
#'
#' @description
#' Fit splines to the data and interpolate the missing observations.
#'
#' @param x a numeric. The x variable.
#' @param y a numeric. The y variable.
#' @param min_obs an integer(1). Mininum number of valid observations to fit a
#' model.
#'
#' @return A numeric with the same lenght as the input data.
#'
#' @export
#'
splines_interpolation <- function(x, y, min_obs = 4) {
  if (any(length(x) < min_obs, length(y) < min_obs))
    return(rep(x = NA_real_, times = length(x)))
  train_df <- data.frame(x = x, y = y)
  train_df <- train_df[stats::complete.cases(train_df), ]
  if (nrow(train_df) < min_obs)
    return(rep(x = NA_real_, times = length(x)))
  m <- stats::smooth.spline(
    x = train_df[["x"]],
    y = train_df[["y"]]
  )
  p <- stats::predict(m, x = x)
  return(p[["y"]])
}
