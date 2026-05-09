test_that("check_formula_library validates correctly", {

  # Create a valid formula library
  valid_formula_library <- data.table::data.table(
    mf = c("C10H20O5", "C12H18O3", "C18H30O6"),
    `12C` = as.integer(c(10, 12, 18)),
    `1H` = as.integer(c(20, 18, 30)),
    `16O` = as.integer(c(5, 3, 6)),
    mass = c(198.2375, 242.4, 312.41)
  )

  # Test that a valid library passes
  # expect_silent(check_formula_library(formula_library = valid_formula_library))

  # Test for missing columns
  invalid_missing_col <- copy(valid_formula_library)
  invalid_missing_col[, `12C` := NULL]
  expect_error(check_formula_library(formula_library = invalid_missing_col),
               "is missing required columns:")

  # Test for incorrect column types
  invalid_col_type <- copy(valid_formula_library)
  invalid_col_type[, `12C` := as.character(`12C`)]
  expect_error(check_formula_library(invalid_col_type),
               "have wrong type")

  # Test for negative element counts
  invalid_negative_count <- copy(valid_formula_library)
  invalid_negative_count[1, `12C` := -1]
  expect_error(check_formula_library(invalid_negative_count),
               "Negative values in.")

  # Test for non-positive mass values
  invalid_mass <- copy(valid_formula_library)
  invalid_mass[1, mass := 0]
  expect_error(check_formula_library(invalid_mass),
               "Non-positive masses ")

  # Test for duplicate vkeys
  invalid_duplicate_vkey <- copy(valid_formula_library)
  invalid_duplicate_vkey[, vkey := .I]
  invalid_duplicate_vkey[2, vkey := 1]
  expect_error(check_formula_library(invalid_duplicate_vkey),
               "Duplicate entries found in 'vkey'")

  # Test for rows with missing mass
  missing_mass <- copy(valid_formula_library)
  missing_mass[2, mass := NA]
  expect_warning(check_formula_library(missing_mass),
                 "Rows with missing 'mass' were")
  expect_equal(nrow(missing_mass[!is.na(mass)]), nrow(valid_formula_library) - 1)

  # Test for unexpected characters in molecular formulas
  invalid_mf <- copy(valid_formula_library)
  invalid_mf[1, mf := "C10H20O5@"]
  expect_warning(check_formula_library(invalid_mf),
                 " contain unexpected characters.")
})
