
# Unit tests for the calc_ma function
test_that("calc_ma calculates mass accuracy correctly", {
  # Basic test with valid example values
  expect_equal(calc_ma(m = 264.08641, m_cal = 264.08653),-0.4544)

  # Test with larger numbers
  expect_equal(calc_ma(m = 1000.5, m_cal = 1000.0), 500)

  # Test with smaller numbers
  expect_equal(calc_ma(m = 0.00001, m_cal = 0.00002),-500000)

  # Test where measured and calculated mass are equal (expect 0 mass accuracy)
  expect_equal(calc_ma(m = 264.08653, m_cal = 264.08653), 0)
})

# Test for invalid inputs: numeric checks and positive values
test_that("calc_ma handles invalid inputs correctly", {
  # Test with non-numeric input
  expect_error(calc_ma(m = "a", m_cal = 264.08653), "numeric")
  expect_error(calc_ma(m = 264.08641, m_cal = "b"), "numeric")

  # Test with values <= 0 (should throw an error)
  expect_error(calc_ma(m = -1, m_cal = 264.08653), "greater than 0")
  expect_error(calc_ma(m = 264.08641, m_cal = 0), "greater than 0")
  expect_error(calc_ma(m = 0, m_cal = 264.08653), "greater than 0")

  # Test with NA input values (should still throw numeric error)
  expect_error(calc_ma(m = NA, m_cal = 264.08653), "numeric")
  expect_error(calc_ma(m = 264.08641, m_cal = NA), "numeric")
})

# Test for missing inputs
test_that("calc_ma throws error for missing inputs", {
  # Test when required arguments are missing
  expect_error(calc_ma(m_cal = 264.08653), "argument \"m\" is missing")
  expect_error(calc_ma(m = 264.08641), "argument \"m_cal\" is missing")
})
