test_that("data type constants are valid", {

  valid_types <- c("logical", "integer", "numeric", "double", "complex",
                   "character", "raw")

  expect_true(all(BRIEFCASE.COLNAMES %in% valid_types))
  expect_true(all(RAWDATA.COLNAMES %in% valid_types))
  expect_true(all(TRAJECTORY.COLNAMES %in% valid_types))
  expect_true(all(TRAJECTORY.METADATA %in% valid_types))

})
