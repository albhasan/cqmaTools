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

