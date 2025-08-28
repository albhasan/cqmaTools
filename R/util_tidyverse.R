#' @title Convert a vector to a tibble
#' @name vector_to_tibble
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description Convert a named vector (or a list of them) into a tibble, using
#' their names as column names.
#
#' @param v a named numeric vector or a list of them.
#'
#' @return a tibble.
#'
#' @export
#'
vector_to_tibble <- function(v) {
  if (is.vector(v, mode = "numeric")) {
    tb <- tibble::tibble_row(v)
    names(tb) <- names(v)
    return(tb)
  } else if (is.list(v)) {
    return(
      lapply(
        X = v,
        FUN = vector_to_tibble
      )
    )
  }
  stop("Invalid input!")
}
