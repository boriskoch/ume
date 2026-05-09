# Test cases
test_that("convert_molecular_formula_to_data_table works correctly", {

  # Test valid molecular formulas
  molecular_formulas <- c("C10H23NO4", "C10H24N4O2S", "C6[13C2]H12[18O2]ONaCl")
  result <- convert_molecular_formula_to_data_table(molecular_formulas)

  # Test that the result is a data.table
  expect_true(is.data.table(result))

  # Test that we have the correct number of rows (3 formulas)
  expect_equal(nrow(result), 3)

  # Test that the 'mf' column exists and is of character type
  expect_true("mf" %in% colnames(result))
  expect_type(result$mf, "character")

  # Test that element counts are correctly calculated (you would need to verify the expected counts for these examples)
  expect_true(all(result$C >= 0))  # Checking that there are counts for Carbon (C)
  expect_true(all(result$H >= 0))  # Checking that there are counts for Hydrogen (H)

  # Test that the warning is raised for duplicates
  molecular_formulas_with_duplicates <- c("C10H23NO4", "C10H23NO4", "C6H12O6")
  expect_warning(
    convert_molecular_formula_to_data_table(molecular_formulas_with_duplicates),
    "Some formulas contain repeated element"
  )

  # Test for invalid element (should stop)
  invalid_formula <- c("C10H23NO4Zz")  # Assuming 'Zz' is not a valid element
  expect_error(
    convert_molecular_formula_to_data_table(invalid_formula),
    "Some formulas could not be parsed"
  )

  # Test isotopic notation handling
  isotopic_formula <- c("C6[13C2]H12[18O2]ONaCl")
  result_iso <- convert_molecular_formula_to_data_table(isotopic_formula)

  # Test that isotopic formulas are handled correctly
  expect_true("13C" %in% colnames(result_iso))
  expect_true("18O" %in% colnames(result_iso))

  # Test the 'mass' column to ensure it's calculated correctly
  expect_true("mass" %in% colnames(result))
  expect_true(all(result$mass > 0))  # Check that mass is positive

  # Test the output when 'table_format' is 'long'
  result_long <- convert_molecular_formula_to_data_table(molecular_formulas, table_format = "long")
  expect_true(is.data.table(result_long))

  # Test that 'long' format includes the 'element' and 'count' columns
  expect_true("symbol" %in% colnames(result_long))
  expect_true("count" %in% colnames(result_long))

})

# Test edge case: empty input vector
# test_that("convert_molecular_formula_to_data_table handles empty input", {
#   result_empty <- convert_molecular_formula_to_data_table(c())
#   expect_equal(nrow(result_empty), 0)
# })

# Test edge case: input with NA or empty strings
test_that("convert_molecular_formula_to_data_table handles NA or empty strings", {
  expect_error(convert_molecular_formula_to_data_table(c(NA, "")), "'mf' must be provided.")
})

test_that("unspecified elements use most abundant isotope by default", {
  res <- convert_molecular_formula_to_data_table("Mo")

  expect_true("98Mo" %in% names(res))
  expect_equal(res[["98Mo"]], 1L)
  expect_false("92Mo" %in% names(res))
  expect_equal(res$nm, 98)
  expect_equal(res$mass, masses[label == "98Mo", exact_mass])
})

test_that("unspecified elements can use lightest isotope", {
  res <- convert_molecular_formula_to_data_table(
    "Mo",
    isotope_default = "lightest"
  )

  expect_true("92Mo" %in% names(res))
  expect_equal(res[["92Mo"]], 1L)
  expect_false("98Mo" %in% names(res))
  expect_equal(res$nm, 92)
  expect_equal(res$mass, masses[label == "92Mo", exact_mass])
})

test_that("explicit isotope notation overrides isotope_default", {
  res1 <- convert_molecular_formula_to_data_table(
    "[92Mo]",
    isotope_default = "most_abundant"
  )

  res2 <- convert_molecular_formula_to_data_table(
    "[92Mo]",
    isotope_default = "lightest"
  )

  expect_true("92Mo" %in% names(res1))
  expect_true("92Mo" %in% names(res2))
  expect_equal(res1[["92Mo"]], 1L)
  expect_equal(res2[["92Mo"]], 1L)
  expect_equal(res1$mass, masses[label == "92Mo", exact_mass])
  expect_equal(res2$mass, masses[label == "92Mo", exact_mass])
})

test_that("multi-letter element symbols without count are parsed correctly", {
  res <- convert_molecular_formula_to_data_table("MgClMo")

  expect_equal(res[["24Mg"]], 1L)
  expect_equal(res[["35Cl"]], 1L)
  expect_equal(res[["98Mo"]], 1L)
  expect_equal(res$mf, "ClMgMo")
})

test_that("multi-letter element symbols with counts are parsed correctly", {
  res <- convert_molecular_formula_to_data_table("Mg2Cl3Mo4")

  expect_equal(res[["24Mg"]], 2L)
  expect_equal(res[["35Cl"]], 3L)
  expect_equal(res[["98Mo"]], 4L)
  expect_equal(res$mf, "Cl3Mg2Mo4")
})

test_that("Some formulas contain", {
  res <- convert_molecular_formula_to_data_table(
    "Mg2Cl3H4C5[37Cl2][37Cl4]Mo",
    isotope_default = "most_abundant"
  )

  expect_true("98Mo" %in% names(res))
  expect_equal(res[["98Mo"]], 1L)
  expect_equal(res[["37Cl"]], 6L)
  expect_equal(res[["35Cl"]], 3L)
  expect_equal(res[["24Mg"]], 2L)
  expect_equal(res[["12C"]], 5L)
  expect_equal(res[["1H"]], 4L)
})

test_that("invalid isotope_default is rejected", {
  expect_error(
    convert_molecular_formula_to_data_table("Mo", isotope_default = "heaviest"),
    "'arg' should be one of"
  )
})
