test_that("ydec2date works", {

    ydec <- 2000.0013661202
    expect_equal(
        ydec2date (ydec),
        expected = "2000-01-01 11:59:59"
    )

})
