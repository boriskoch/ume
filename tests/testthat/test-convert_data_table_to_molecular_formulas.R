# Unit test
test_that("convert_data_table_to_molecular_formulas works correctly", {

  test_molecular_formulas <- data.table(
    `12C` = c(1, 2, 3, 4, 5, 6),
    `13c` = c(0, 1, 0, 1, 0, 1),
    `1H` = c(4, 4, 5, 6, 8, 10)
  )

  # Case 1: Regular molecular formula without isotopes
  result <- convert_data_table_to_molecular_formulas(mfd = test_molecular_formulas)
  result
  expect_true("mf" %in% colnames(result), "The 'mf' column should exist in the output data.table.")

  # Test output for correct molecular formula format (Hill order, no isotopes)
  #expect_equal(result$mf[1], "C10H23NO4", "The molecular formula for C10H23NO4 is incorrect.")
  #expect_equal(result$mf[2], "C10H24N4O2S", "The molecular formula for C10H24N4O2S is incorrect.")

  # Case 2: Molecular formula with isotopes
  result_iso <- convert_data_table_to_molecular_formulas(test_molecular_formulas, isotope_formulas = TRUE)

  expect_true("mf_iso" %in% colnames(result_iso), "The 'mf_iso' column should exist in the output data.table.")

  # Test output for correct molecular formula with isotopes
  #expect_equal(result_iso$mf_iso[3], "[13C2][1H12][18O2][1O][1Na][1Cl]", "The molecular formula with isotopes for C6[13C2]H12[18O2]ONaCl is incorrect.")

  # Case 3: Empty input
  #empty_dt <- data.table()
  #result_empty <- convert_data_table_to_molecular_formulas(empty_dt, isotope_formulas = FALSE)

  #expect_true(nrow(result_empty) == 0, "The function should return an empty data.table when input is empty.")

  # Case 4: Input without any element columns
  #invalid_dt <- data.table(vkey = 1:3)
  #result_invalid <- convert_data_table_to_molecular_formulas(invalid_dt, isotope_formulas = FALSE)

  #expect_true(nrow(result_invalid) == 3, "The function should still return the original data.table even if no element columns are present.")
  #expect_equal(result_invalid$mf, rep(NA, 3), "The molecular formula for invalid input should be NA.")
})

