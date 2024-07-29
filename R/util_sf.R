#' Get the geometry column name
#'
#' @description
#' Get the names of the geometry columns in an sf object.
#'
#' @param x An sf object.
#'
#' @return  A character. 
#'
get_geom_colname <- function(x){
    stopifnot("An sf object was expected!" = 
        inherits(x, what = "sf"))
    return(names(which(sapply(x, inherits, what = "sfc"))))
}

