#' Read raw data from a file
#'
#' @description
#' Read a file with data from the results of LaGEE analysis of GHGs.
#'
#' @param file_path A character. Path to a data file.
#' @param cnames A character. Column names.
#' @param ctypes A character. Column types.
#' @param skip An integer. Number of lines to skip when reading the file.
#'
#' @return a data frame.
#'
#' @export
#'
read_rawdata_file <- function(file_path,
                              cnames = names(RAWDATA.COLNAMES),
                              ctypes = RAWDATA.COLNAMES,
                              skip = RAWDATA.SKIP) {

  data_df <- utils::read.table(
    file = file_path,
    sep = "",
    header = FALSE,
    skip = skip,
    col.names = cnames,
    colClasses = ctypes
  )

  return(data_df)

}
