library(testthat)

test_that("calc_simpson_index works as expected", {
  # Test case 1: Basic functionality
  mf <- c("C10H20O5", "C12H18O3", "C18H30O6")
  magnitude <- c(1982375, 2424, 312410)
  result <- calc_simpson_index(mf, magnitude)
  expect_type(result, "double")
  expect_true(result > 0)

  # Test case 2: Single molecular formula
  mf_single <- c("C10H20O5")
  magnitude_single <- c(100)
  result_single <- calc_simpson_index(mf_single, magnitude_single)
  expect_equal(result_single, 1) # Only one formula, so index is 1

  # Test case 3: Zero abundance
  mf_zero <- c("C10H20O5", "C12H18O3")
  magnitude_zero <- c(0, 0)
  result_zero <- calc_simpson_index(mf_zero, magnitude_zero)
  expect_equal(result_zero, 0)

  # Test case 4: Equal abundances
  mf_equal <- c("C10H20O5", "C12H18O3", "C18H30O6")
  magnitude_equal <- c(100, 100, 100)
  result_equal <- calc_simpson_index(mf_equal, magnitude_equal)
  expect_true(result_equal > 0)

  # Test case 5: Invalid inputs
  expect_error(calc_simpson_index(mf = NULL, magnitude = c(1, 2, 3)), "'mf' must be a non-empty character vector.")
  expect_error(calc_simpson_index(mf = c("C10H20O5"), magnitude = c(-1)), "'magnitude' must contain non-negative values.")
  expect_error(calc_simpson_index(mf = c("C10H20O5"), magnitude = NULL), "'magnitude' must be a numeric vector")
  expect_error(calc_simpson_index(mf = c("C10H20O5"), magnitude = c(1, 2)), "same length as 'mf'")
})
