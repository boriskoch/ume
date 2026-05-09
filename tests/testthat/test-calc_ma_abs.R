
# Unit tests for the calc_ma_abs function
test_that("calc_ma_abs calculates correct mass limits", {

  # Test with valid example values
  result <- calc_ma_abs(m = 264.08641, ma_dev = 5)
  expect_equal(result$m_min, 264.08641 - (264.08641 * 5 / 1000000))
  expect_equal(result$m_max, 264.08641 + (264.08641 * 5 / 1000000))

  # Test with larger numbers
  result <- calc_ma_abs(m = 1000.5, ma_dev = 10)
  expect_equal(result$m_min, 1000.5 - (1000.5 * 10 / 1000000))
  expect_equal(result$m_max, 1000.5 + (1000.5 * 10 / 1000000))
})

# Test for invalid inputs: numeric checks and positive values
test_that("calc_ma_abs handles invalid inputs correctly", {

  # Non-numeric inputs should throw an error
  expect_error(calc_ma_abs(m = "a", ma_dev = 5), "numeric")
  expect_error(calc_ma_abs(m = 264.08641, ma_dev = "b"), "numeric")

  # Negative or zero values should throw an error
  expect_error(calc_ma_abs(m = -1, ma_dev = 5), "greater than 0")
  expect_error(calc_ma_abs(m = 264.08641, ma_dev = 0), "greater than 0")
  expect_error(calc_ma_abs(m = 0, ma_dev = 5), "greater than 0")

  # Test with NA values
  expect_error(calc_ma_abs(m = NA, ma_dev = 5), "numeric")
  expect_error(calc_ma_abs(m = 264.08641, ma_dev = NA), "numeric")
})

# Test for high ma_dev values
test_that("calc_ma_abs gives a warning for ma_dev values greater than 100", {
  expect_message(calc_ma_abs(m = 264.08641, ma_dev = 150),
    "ma_dev is the relative mass accuracy given in ppm")
})

# Test for missing inputs
test_that("calc_ma_abs throws error for missing inputs", {
  # Test when required arguments are missing
  expect_error(calc_ma_abs(ma_dev = 5), "argument \"m\" is missing")
  expect_error(calc_ma_abs(m = 264.08641), "argument \"ma_dev\" is missing")
})
