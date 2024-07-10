test_that("listnames2dataframes works", {

    # Create data frames with random data on them.
    n <- sample.int(10, size = 1)
    df_names <- paste0("df_", seq(n))
    df_ls <- lapply(df_names, function(x){
        dims <- sample.int(10, size = 2)
        data.frame(matrix(rnorm(n = prod(dims)), nrow = dims[1]))
    })
    names(df_ls) <- df_names

    # Add the list's names as a new column in each data frame.
    cname <- "new_column"
    df_ls <- listnames2dataframes(df_ls, cname = cname)

    # Test that the new column must be present on each data frame.
    expect_true(
    all(vapply(df_ls, function(x){cname %in% colnames(x)}, logical(1)))
    )

    # Test that the new column values must match the list's names.
    expect_true(
        all(vapply(df_ls, function(x){unique(x[[cname]])}, character(1)) %in% 
            df_names)
    )

})

test_that("files2df works", {

    # List trajectory files.
    files <- list.files(
        path = system.file("extdata", "trajectories", "2011", 
                           package = "cqmaTools"),
        pattern = "^[A-Z]{3}_[0-9]{4}_[0-9]{2}_[0-9]{2}_[0-9]+.[0-9]+",
        full.names = TRUE
    )
    if (length(files) > 10)
        files <- files[sample(1:length(files))[1:10]]

    df_ls <- files2df(
        files = files, 
        header = FALSE, 
        skip = 7, 
        cnames = HYSPLIT.COLNAMES)

    # The list's length must match the number of files.
    expect_equal(length(df_ls), expected = length(files))

    # The list's names must match the file names.
    expect_equal(names(df_ls), expected = basename(files))

    # The list's elements must be data frames.
    expect_true(all(vapply(df_ls, is.data.frame, logical(1))))

})

test_that("filter_data_frames works", {

    # Create data frames, half with positive and half with negative numbers.
    df_ls <- lapply(seq(sample.int(10, size = 1)), function(x) {
        dims <- sample.int(10, size = 2)
        data_df <- data.frame(matrix(rnorm(n = prod(dims)), nrow = dims[1]))
        data_df <- abs(data_df)
        if (x %% 2 == 1) {
            data_df <- data_df * (-1)
        }
        colnames(data_df) <- paste0("X", seq(data_df))
        return(data_df)
    })
    stopifnot("Expected numeric columns!" = all(
        vapply(df_ls, function(x){
            all(vapply(x, is.numeric, logical(1)))
        }, logical(1))
    ))

    # Filter none.
    r <- range(vapply(df_ls, range, numeric(2)))
    filter_ls <- filter_data_frames(
        x = df_ls,
        cname = "X1",
        min = r[1],
        max = r[2]
    )
    expect_true(length(filter_ls) == length(df_ls))

    # Error: The given column doesn't exist.
    expect_error(
        filter_ls <- filter_data_frames(
            x = df_ls, 
            cname = paste(LETTERS, collapse = ""), 
            min = 0, 
            max = Inf
        )
    )

    # Filter out the negative data frames.
    # Approximately half of the data frames must be filtered out
    filter_ls <- filter_data_frames(
        x = df_ls,
        cname = "X1",
        min = 0,
        max = Inf
    )
    expect_true(length(filter_ls) * 2 <= length(df_ls))

    # Filter out the positive data frames.
    # Approximately half of the data frames must be filtered out
    filter_ls <- filter_data_frames(
        x = df_ls,
        cname = "X1",
        min = -Inf,
        max = 0
    )
    expect_true(length(filter_ls) * 2 <= (length(df_ls) + 2))

    # Filter all.
    filter_ls <- filter_data_frames(
        x = df_ls,
        cname = "X1",
        min = 0,
        max = 0
    )
    expect_true(length(filter_ls) == 0)

})



test_that("cast_df_cols works", {

    # Create data frames with random character data.
    rows_cols <- sample.int(10, size = 2)
    cnames <- sample(c("character", "integer", "double"), size = rows_cols[2], 
                     replace = TRUE)
    names(cnames) <- paste0("df_", seq(rows_cols[2]))
    data_frame <- lapply(cnames, function(x){
        if (x == "character") 
            res <- sample(LETTERS, size = rows_cols[1], replace = TRUE)
        if (x == "double") 
            res <- rnorm(n = rows_cols[1])
        if (x == "integer") 
            res <- sample(10, size = rows_cols[1], replace = TRUE)
        return(res)
    })
    data_frame <- data.frame(lapply(data_frame, as.character))

    # Cast column to original types.
    res <- cast_df_cols(data_frame, cnames)
    res_cnames <- vapply(res, class, character(1))

    # Test the casting result.
    exp_cnames <- cnames
    exp_cnames[exp_cnames == "double"] <- "numeric"
    expect_equal(res_cnames, expected = exp_cnames)
    expect_equal(names(res_cnames), expected = names(exp_cnames))

    # Test errors.
    expect_error(cast_df_cols(data_frame, LETTERS))
    expect_error(cast_df_cols(data_frame, LETTERS[seq(rows_cols[2])]))

})

