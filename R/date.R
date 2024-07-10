#' Transform a decimal date to a date-format string
#'
#' @description
#' Transform a decimal-year date into a date-format string
#'
#' @param ydec a numeric(1) representing a date as a decimal year. e.g. 
#'   2000.0013661202.
#'
#' @return         A character. e.g. 2000-01-01 11:59:59.999410
#'
ydec2date <- function(ydec){
      return(as.character(lubridate::date_decimal(ydec)))
}

