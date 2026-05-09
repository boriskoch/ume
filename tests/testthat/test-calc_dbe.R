
# Start writing tests
test_that("calc_dbe calculates correct DBE values for typical molecules", {
  # Example molecules with known DBE values
  mfd <- data.table(c = c(2, 6), h = c(4, 6), o = c(1, 0)) # Water and ethylene
  result <- calc_dbe(mfd = mfd)

  # DBE calculations:
  # - C2H4O (ethanol): DBE = 0
  # - C2H6 (ethane): DBE = 0
  expect_equal(result[1], 1, tolerance = 1e-8)
  expect_equal(result[2], 4, tolerance = 1e-8)
})

test_that("calc_dbe returns correct DBE for complex formulas", {
  # Testing a more complex molecule, e.g., benzene (C6H6) with known DBE
  mfd <- data.table(c = 6, h = 6)
  result <- calc_dbe(mfd = mfd)

  # Expected DBE for benzene (C6H6) is 4 (three double bonds and one ring)
  expect_equal(result, 4, tolerance = 1e-8)
})

test_that("calc_dbe handles elements with valence 2 correctly", {
  # Adding sulfur with valence 2 to verify exclusion
  mfd <- data.table(c = 3, h = 6, s = 1)
  result <- calc_dbe(mfd = mfd)

  # C3H6S should have DBE of 1, sulfur does not affect DBE
  expect_equal(result, 1, tolerance = 1e-8)
})

# test_that("calc_dbe throws an error if a missing valence is detected", {
#   # Simulate missing valence by creating a mock masses data.table with valence NA
#   mock_masses <- ume::masses
#   mock_masses[element == "c", valence := NA]
#
#   # Expect an error due to missing valence for 'c'
#   expect_error(calc_dbe(mfd = data.table(c = 2, h = 4), masses = mock_masses),
#                "Valence of element is missing in masses.Rdata!")
# })

test_that("calc_dbe handles empty input gracefully", {
  # Empty input data.table
  mfd <- data.table()

  # Result should be an empty numeric vector
  expect_error(calc_dbe(mfd = mfd), "Input data.table 'mfd' is empty. Please provide a non-empty data.table.")
})

test_that("calc_dbe works with multiple rows", {
  # Test multiple rows with different molecules
  mfd <- data.table(c = c(2, 6, 5), h = c(4, 6, 8), o = c(0, 0, 2))
  result <- calc_dbe(mfd = mfd)

  # Expected DBE for each row: C2H4 = 1, C6H6 = 4, C5H8O2 = 2
  expect_equal(result, c(1, 4, 2), tolerance = 1e-8)
})

