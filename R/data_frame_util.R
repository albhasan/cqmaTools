#' Add the list's names to the data frames in the list
#'
#' @description
#' Given a list of data frames, add each list's name to its corresponding data frame.
#'
#' @param df_ls a named list of data frames.
#' @param cname a character(1). The name for the new column.
#'
#' @return         A list of data frames.
#' 
listnames2dataframes <- function(df_ls, cname){

    stopifnot("Expected a list of data frames!" = 
              all(vapply(df_ls, is.data.frame, logical(1))))
    stopifnot("Expected a named list!" = 
              length(names(df_ls)) == length(df_ls))

    res <- lapply(seq(df_ls), function(x, df_ls){
            df_ls[[x]][cname] <- names(df_ls)[x]
            return(df_ls[[x]])
        }, 
        df_ls = df_ls
    )

    return(res)

}


#' Read files into data frames
#'
#' @description 
#' Read the given files into data frames (one per file).
#'
#' @param files a character. Paths to files.
#' @param header a logical(1). Do the files have a header row?
#' @param skip a numeric(1). Number of lines to skip from each file.
#' @param cnames a character. Column names for the data in files.
#'
#' @return a list of data frames. 
#'
#' @export
#'
files2df <- function(files, header, skip, cnames) {

    if(length(files) == 0){
        warning("Empty list")
        return(list())
    }
    stopifnot("File(s) doesn't exist!" = 
        vapply(files, file.exists, logical(1))
    )

    data_df_ls <- lapply(files, function(x) {
        data_df <- utils::read.table(
            file = x, 
            sep = "", 
            header = header,
            skip = skip
        )
        if (!header)
            colnames(data_df) <- cnames
        return(data_df)
    })
    names(data_df_ls) <- basename(files)

    return(data_df_ls)
}



#' Filter data frames
#'
#' @description
#' Remove data frames from a list when their rows don't fall inside the given
#' numeric range for the given column.
#'
#' @param x either a data frame or a list of data frames.
#' @param cname a character(1). A column name in each of the given data frames.
#' @param min,max numeric(1). Maximum and mininum values.
#'
#' @details
#' When x is a single data frame and it doesn't meet the filter, this function
#' returns NA.
#'
#' @return a data frame or a list of data frames or NA (see details).
#'
#' @export
#'
filter_data_frames <- function(x, cname, min, max) {

    f <- function(y) {
        stopifnot("Column not found!" = cname %in% colnames(y))
        if (!is.data.frame(y)) return(FALSE)
        if (!all(nrow(y) > 0, ncol(y) > 0)) return(FALSE)
        if (!is.numeric(y[[cname]])) stop("Numeric column expected!")
        if (all(min == -Inf, max == Inf)) return(TRUE)
        if(!all(all(y[[cname]] >= min), all(y[[cname]] <= max))) return(FALSE)
        return(TRUE)
    }

    if (is.data.frame(x)) {
        if (f(x)) {
            return(x)
        } else {
            return(NA)
        }
    } else if (is.list(x)) {
        minmax <- vapply(x, f, logical(1))
        minmax[is.na(minmax)] <- FALSE
        return(x[minmax])
    }

    stop("Invalid argument. Expected a data frame or a list!")
}

