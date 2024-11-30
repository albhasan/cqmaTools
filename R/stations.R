#' Read metereological data from a station
#' 
#' @description
#' Read a file with metereological data.
#'
#' @param file_path A character. Path to a data file.
#' 
#' @return a data.frame with 2 columns: date and value.
#' 
#' @export
#'
read_station_file <- function(file_path) {
    return(
        utils::read.table(
            file = file_path,
            sep = "",
            header = FALSE,
            col.names = c("date", "value")
        )
    )
}

