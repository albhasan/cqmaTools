test_that("moving_window works", {

  # A hsize 0 must return the input vector.
  x <- rnorm(n = 10)
  res <- moving_window(x = x, f = mean, hsize = 0, na.rm = FALSE)
  expect_true(all(x == res))
  x <- rnorm(n = 19)
  res <- moving_window(x = x, f = mean, hsize = 0, na.rm = TRUE)
  expect_true(all(x == res))

  # Compare to similar functions.
  x <- rnorm(n = 17, sd = 3) * 10
  hsize <- sample.int(n = length(x) / 2, size = 1)
  res <- moving_window(x = x, f = mean, hsize = hsize, na.rm = FALSE)
  expected <- zoo::rollapply(data = x, FUN = mean, align = "center",
                             width = (hsize * 2) + 1, fill = NA, na.rm = TRUE)
  pos <- c((hsize + 1):(length(x) - hsize))
  expect_true(all(res[pos] == expected[pos]))

  # Handle a NA vector.
  len_x <- 11
  x <- rep(NA_real_, times = len_x)
  hsize <- sample.int(n = len_x, size = 1)
  res <- moving_window(x = x, f = mean, hsize = hsize, na.rm = TRUE)
  expect_true(all(is.na(res)))
  res <- moving_window(x = x, f = mean, hsize = hsize, na.rm = TRUE)
  expect_true(all(is.na(res)))

  # Handle a single NA.
  len_x <- 11
  x <- rnorm(n = 17, sd = 3) * 10
  hsize <- sample.int(n = length(x), size = 1)
  holes <- sample.int(n = length(x), size = 1)
  x[holes] <- NA
  res <- moving_window(x = x, f = mean, hsize = hsize, na.rm = TRUE)
  expect_true(sum(is.na(res)) == 0)

  # Handle NA holes.
  x <- 1.0:11.0
  holes <- sample.int(n = length(x), size = 3)
  x[holes] <- NA
  hsize <- sample.int(n = length(x) / 4, size = 1)
  res <- moving_window(x = x, f = mean, hsize = hsize, na.rm = TRUE)
  expected <- zoo::rollapply(data = x, FUN = mean, align = "center",
                             width = (hsize * 2) + 1, fill = NA, na.rm = TRUE)

})

test_that("splines_interpolation works", {

  # # Interpolation results should be very close to the input observations.
  # # NOTE: splines don't preserve the training values!
  # are_close <- function(x, y, tolerance = .Machine$double.eps ^ 0.5) {
  #   return(abs(y - x) < tolerance)
  # }
  # len <- sample.int(n = 17, size = 1)
  # x <- rnorm(n = len)
  # y <- rnorm(n = len)
  # res <- splines_interpolation(x = x, y = y, min_obs = 4)
  # expect_true(all(are_close(x = res, y = y)))

  # x shouldn't have missing values.
  len <- 17
  x <- rnorm(n = len)
  y <- rnorm(n = len)
  holes <- sample.int(n = length(x), size = 1)
  x[holes] <- NA
  expect_error(splines_interpolation(x = x, y = y, min_obs = 4))

  # Interpolation results shouln't have missing values.
  len <- 19
  x <- rnorm(n = len)
  y <- rnorm(n = len)
  holes <- sample.int(n = length(y), size = 1)
  y[holes] <- NA
  res <- splines_interpolation(x = x, y = y, min_obs = 4)
  expect_true(sum(is.na(res)) == 0)

  # x & y should have the same lenght.
  len <- 11
  x <- rnorm(n = len)
  y <- rnorm(n = length(x) + 1)
  expect_error(splines_interpolation(x = x, y = y, min_obs = 4))

  # Less observations than the mininum should return an NA vector.
  len <- 11
  x <- rnorm(n = len)
  y <- rnorm(n = len)
  res <- splines_interpolation(x = x, y = y, min_obs = 13)
  expect_true(sum(is.na(res)) == length(res))

})
