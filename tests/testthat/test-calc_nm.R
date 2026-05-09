test_that("calc_nm correctly calculates nominal mass for molecular formulas", {

  # Prepare a sample input data.table with known molecular formulas
  test_data <- data.table::data.table(
    mf = c("C6H12O6", "C2H5OH", "CH4", "H2O"),
    c = c(6, 2, 1, 0),
    h = c(12, 6, 4, 2),
    o = c(6, 1, 0, 1)
  )

  # Expected nominal masses for the above molecular formulas
  # Based on integer atomic masses: C = 12, H = 1, O = 16
  expected_nm <- c(180, 46, 16, 18)

  # Run calc_nm function on the test data
  result <- test_data[, nm:=calc_nm(mfd = test_data)]

  # Check that the "nm" column is correctly calculated
  expect_true("nm" %in% colnames(result), info = "Output should contain 'nm' column")

  # Verify the nominal mass values match the expected values
  expect_equal(result$nm, expected_nm, tolerance = 0,
               info = "Nominal mass values should match expected results")

  # Check that the original columns are preserved
  expect_equal(colnames(result), c(colnames(test_data)),
               info = "All original columns should be preserved in the output")

  # Edge case: Empty input data.table should return an empty result with 'nm' column
  empty_data <- data.table::data.table(mf = character(), c = integer(), h = integer(), o = integer())

  expect_error(calc_nm(mfd = empty_data), "Input data.table 'mfd' is empty. Please provide a non-empty data.table.")

  # Edge case: Single atom molecular formula
  single_atom_data <- data.table::data.table(mf = "H", h = 1)
  single_atom_result <- single_atom_data[, nm:=calc_nm(mfd = single_atom_data)]
  expect_equal(single_atom_result$nm, 1,
               info = "Nominal mass for single hydrogen atom should be 1")

  # Test the calculation with a molecular formula string:
  formula_mass <- calc_nm(mfd = c("C2H4", "C2H4Cl"))
  expect_equal(formula_mass, c(28, 63))

})

