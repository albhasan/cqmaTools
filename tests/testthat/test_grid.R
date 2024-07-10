test_that("grid_helper works", {

    o = 0
    min = -10
    max = 10
    res = 2
    x <- grid_helper(o = o, min = min, max = max, res = res)
    expect_true(o %in% x)
    expect_equal(x, expected = seq(from = min, to = max, by = res))
    expect_true(all(o >= x[1], o <= x[length(x)]))
    expect_true(all(min <= x[1], x[length(x)] <= max))

    o = 0
    min = -9
    max = 7
    res = 1
    x <- grid_helper(o = o, min = min, max = max, res = res)
    expect_true(o %in% x)
    expect_equal(x, expected = seq(from = min, to = max, by = res))
    expect_true(all(o >= x[1], o <= x[length(x)]))
    expect_true(all(min <= x[1], x[length(x)] <= max))

    o = 6.5
    min = -9
    max = 7
    res = 1
    x <- grid_helper(o = o, min = min, max = max, res = res)
    expect_true(o %in% x)
    expect_true(all(o >= x[1], o <= x[length(x)]))
    expect_true(all(min <= x[1], x[length(x)] <= max))

})



test_that("build_grid works", {

    # The origin must be inside the given ranges.
    expect_error(build_grid(origin_lon = 0, origin_lat = 0, min_lon = 10,
        max_lon = 30, min_lat = 40, max_lat = 50, grid_resolution = 1,
        crs = 4326))
    expect_error(build_grid(origin_lon = 0, origin_lat = 0, min_lon = 10,
        max_lon = 30, min_lat = -40, max_lat = 50, grid_resolution = 1,
        crs = 4326))
    expect_error(build_grid(origin_lon = 0, origin_lat = 0, min_lon = -10,
        max_lon = 30, min_lat = 40, max_lat = 50, grid_resolution = 1,
        crs = 4326))

    lon_o = -74
    lat_o = 4
    lon_min = -78
    lon_max = -70
    lat_min = -5
    lat_max = 12
    res = 0.5 
    srs = 4326
    g <- build_grid(origin_lon = lon_o, origin_lat = lat_o, min_lon = lon_min,
        max_lon = lon_max, min_lat = lat_min, max_lat = lat_max,
        grid_resolution = res, crs = 4326)

    # Test the number of cells in the grid.
    expect_equal(nrow(g),
        expected = ((lon_max - lon_min)/res) * ((lat_max - lat_min)/res))

    # Test the origin must be a vertex in the grid.
    expect_true(any(apply(
        X = sf::st_coordinates(sf::st_cast(g, "MULTIPOINT")),
        MARGIN = 1,
        FUN = function(x) { all(x[1] == lon_o, x[2] == lat_o) }
    )))

    lon_o = -23
    lat_o = -45
    lon_min = -27
    lon_max = -15
    lat_min = -50
    lat_max = -40
    res = 0.5 
    srs = 4326
    g <- build_grid(origin_lon = lon_o, origin_lat = lat_o, min_lon = lon_min,
        max_lon = lon_max, min_lat = lat_min, max_lat = lat_max,
        grid_resolution = res, crs = 4326)

    # Test the number of cells in the grid.
    expect_equal(nrow(g),
        expected = ((lon_max - lon_min)/res) * ((lat_max - lat_min)/res))

    # Test the origin must be a vertex in the grid.
    expect_true(any(apply(
        X = sf::st_coordinates(sf::st_cast(g, "MULTIPOINT")),
        MARGIN = 1,
        FUN = function(x) { all(x[1] == lon_o, x[2] == lat_o) }
    )))

})



test_that("grid_to_raster works", {

    lon_o = -74
    lat_o = 4
    lon_min = -78
    lon_max = -70
    lat_min = -5
    lat_max = 12
    res = 0.5 
    srs = 4326
    g_sf <- build_grid(origin_lon = lon_o, origin_lat = lat_o,
        min_lon = lon_min, max_lon = lon_max, min_lat = lat_min,
        max_lat = lat_max, grid_resolution = res, crs = 4326)
    g_sf["val"] <- rnorm(n = nrow(g_sf))

    expect_error(grid_to_raster(grid_sf = g_sf, grid_resolution = res,
                                cname = "fake_name"))

    g_r <- grid_to_raster(grid_sf = g_sf, grid_resolution = res,
                          cname = "val")

    tol <- .Machine$double.eps^0.3
    expect_equal(prod(dim(g_r)), expected = nrow(g_sf))
    expect_true(abs(sum(g_r[]) - sum(g_sf[["val"]])) < tol)
    expect_true(abs(mean(g_r[]) - mean(g_sf[["val"]])) < tol)
    expect_true(abs(sd(g_r[]) - sd(g_sf[["val"]])) < tol)
    expect_true(abs(min(g_r[]) - min(g_sf[["val"]])) < tol)
    expect_true(abs(max(g_r[]) - max(g_sf[["val"]])) < tol)

})

