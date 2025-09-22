#' Transform decimal-years to a date-format string
#'
#' @description
#' Transform decimal-years to a date-format string
#'
#' @param ydec a numeric(1) representing a date as a decimal year. e.g.
#'   2000.0013661202.
#'
#' @return         A character. e.g. 2000-01-01 11:59:59.999410
#'
ydec2date <- function(ydec) {
  return(as.character(lubridate::date_decimal(ydec)))
}



#' Build a date
#'
#' @description
#' Build a date from its parts.
#'
#' @param year,month,day vectors (of the same length) which
#' together constitute a date.
#'
#' @return a date vector.
#'
build_date <- function(year, month, day) {

  .Deprecated(
    new = "as_date",
    package = "lubridate"
  )

  adate <-
    paste(
      year,
      sprintf("%02d", as.integer(month)),
      sprintf("%02d", as.integer(day)),
      sep = "-"
    )

  #NOTE: casting as.POSIXct("2010-10-17 00:00:00") is different from
  #      casting as.POSIXct("2010-10-17 24:00:00")
  #      The results have different format. Better use lubridate.

  adate <- lubridate::as_date(adate)

  return(adate)

}
