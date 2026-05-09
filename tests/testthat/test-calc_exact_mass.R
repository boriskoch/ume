test_that("calc_exact_mass calculates exact mass accurately", {
  # Test data table with common molecule (water H2O)
  mfd <- data.table(h = 2, o = 1)
  result <- mfd[, mass:=calc_exact_mass(mfd = mfd)]

  # Expected exact mass for H2O (2*1.007825 + 15.994915)
  expected_mass <- 2 * 1.007825 + 15.994915

  # Check if the calculated mass matches the expected mass
  expect_equal(result$mass, expected_mass, tolerance = 1e-5)
})

test_that("calc_exact_mass adds mass column to data.table", {
  # Sample data table with molecular formula columns
  mfd <- data.table(c = 1, h = 4) # methane CH4
  result <- mfd[, mass:=calc_exact_mass(mfd = mfd)]

  # Check if the mass column is added
  expect_true("mass" %in% colnames(result))
})

test_that("calc_exact_mass returns an error for empty input", {
  # Empty input data.table
  mfd <- data.table()

  # Expect an error message indicating input is empty
  expect_error(calc_exact_mass(mfd = mfd),
               "Input data.table 'mfd' is empty. Please provide a non-empty data.table.")
})

# Test the calculation with a molecular formula string:
  formula_mass <- calc_exact_mass(mfd = c("C2H4", "C2H4Cl")) |> round(5)
  expect_equal(formula_mass, c(28.03130, 63.00015))

# test_that("calc_exact_mass handles missing isotope information gracefully", {
#   # Data table with isotope not present in ume::masses
#   mfd <- data.table(x = 1)
#
#   # Expect an error about missing isotope information
#   expect_error(calc_exact_mass(mfd = mfd),
#                "Isotope 'x' not found in masses data.")
# })
