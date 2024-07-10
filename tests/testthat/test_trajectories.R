test_that("traj2lines works", {

    files <- list.files(
        path = system.file("extdata", "trajectories", package = "cqmaTools"),
        pattern = "*",
        full.names = TRUE,
        recursive = TRUE
    )
    if (length(files) > 10)
        files <- files[sample(1:length(files))[1:10]]

    df_ls <- files2df(
        files = files,
        header = FALSE,
        skip = 7,
        cnames = HYSPLIT.COLNAMES
    )

    traj_ls <- lapply(df_ls, traj2lines)

    # traj_ls is made of lists. Why? because traj2lines resturns a list.
    expect_true(all(vapply(traj_ls, inherits, what = "list", logical(1))))

    # traj_ls elements are all LINESTRINGs.
    expect_true(all(sapply(traj_ls, function(x) {
        all(vapply(x, inherits, what = "LINESTRING", logical(1)))
    })))

    # The number of LINESTRING is the one less than the number of rows in its
    # data.frame.
    expect_true(all(vapply(df_ls, function(x) {
        length(traj2lines(x)) == nrow(x) - 1
    }, logical(1))))

})



test_that("format_traj_names works", {

    test_names <- c("ALF_2011_12_30_15_4419.60", "ALF_2011_04_16_16_2133.60",
        "ALF_2011_11_02_16_609.60",  "ALF_2011_01_12_16_609.60",
        "ALF_2011_01_24_16_457.20")
    expt_names <- c("ALF_2011_12_30_4419.6_15", "ALF_2011_04_16_2133.6_16",
        "ALF_2011_11_02_0609.6_16", "ALF_2011_01_12_0609.6_16",
        "ALF_2011_01_24_0457.2_16")
    traj_names <- format_traj_names(test_names) 
    expect_equal(traj_names, expected = expt_names)

})



test_that("get_trajectory_metadata works", {

    files <- list.files(
        path = system.file("extdata", "trajectories", package = "cqmaTools"),
        pattern = TRAJECTORY.FILENAME.PATTERN,
        full.names = TRUE,
        recursive = TRUE
    )
    if (length(files) > 10)
        files <- files[sample(1:length(files))[1:10]]

    traj_df <- get_trajectory_metadata(
        files = files,
        cnames = TRAJECTORY.COLNAMES,
        m_period = YEAR.TRIMESTERS
    )

    expect_true(all(nrow(traj_df) <= 10, ncol(traj_df) == 8))
    expect_equal(colnames(traj_df), expected = 
        c("site", "year", "month", "day", "hour",
          "height", "m_period", "filepath"))
    expect_true(all(traj_df[["m_period"]] %in% c("t1", "t2", "t3", "t4")))

    traj_df <- get_trajectory_metadata(
        files = files,
        cnames = TRAJECTORY.COLNAMES,
        m_period = YEAR.SEMESTERS
    )

    expect_true(all(nrow(traj_df) <= 10, ncol(traj_df) == 8))
    expect_equal(colnames(traj_df), expected = 
        c("site", "year", "month", "day", "hour",
          "height", "m_period", "filepath"))
    expect_true(all(traj_df[["m_period"]] %in% c("s1", "s2")))

    # Test errors.
    expect_error( get_trajectory_metadata(
        files = files, cnames = TRAJECTORY.COLNAMES,
        m_period = paste("p", 1:11)
    ))
    expect_error(get_trajectory_metadata(
        files = files, 
        cnames = c(s = 1, y = 2, m = 3, d = 4, h = 5, he = 6),
        m_period = paste("p", 1:11)
    ))
    cnames <- TRAJECTORY.COLNAMES
    cnames["month"] <- "x"
    expect_error(get_trajectory_metadata(
        files = files, cnames = cnames, m_period = paste("p", 1:11)
    ))

})



test_that("intersect_trajectories works", {

    traj_df <- data.frame(
        V1 = rep(1, 5), V2 = rep(1, 5), year = 21, month = 1, day = 3, 
        hour = rev(0:4), min = 0, V8 = rep(0, 5), V9 = rep(0, 5),
        lat = seq(-0, 10, length.out = 5), 
        lon = seq(-70, -60, length.out = 5), 
        height = rnorm(5, mean = 200, sd = 20),
        pressure = rnorm(5, mean = 950, sd = 10)
    )

    limit_mt <- matrix(c(-69, 20, -69, 18), ncol = 2, byrow = TRUE,
        dimnames = list(NULL, c("lon", "lat")))
    limit <- data.frame(id = 1)
    sf::st_geometry(limit) <- 
    sf::st_sfc(sf::st_linestring(limit_mt, dim = "XY"), crs = 4326)

    #intersect_trajectories(traj_df, limit = limit, crs = 4326)

})
