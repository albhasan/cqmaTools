test_that("ydec2date works", {

  ydec <- 2000.0013661202
  expect_equal(
    ydec2date(ydec),
    expected = "2000-01-01 11:59:59.99941"
  )

})

test_that("build_date works", {

  d1 <- build_date(
    year = "2010",
    month = "10",
    day = "17"
  )

  expect_equal(
    object = d1,
    expected = lubridate::as_date("2010-10-17")
  )

})
