#' Read briefcase data from a file
#'
#' @description
#' Read a file with pre-processed metadata from the briefcases.
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
read_briefcase_file <- function(file_path,
                                cnames = names(BRIEFCASE.COLNAMES),
                                ctypes = BRIEFCASE.COLNAMES,
                                skip = BRIEFCASE.SKIP) {

  data_df <- utils::read.table(
    file = file_path,
    sep = " ",
    header = TRUE,
    col.names = cnames,
    colClasses = ctypes,
    skip = skip,
    tryLogical = FALSE,
    check.names = FALSE
  )

  return(data_df)

}
