
# Example sample data for testing
mfd_example <- mf_data_demo
setnames(mfd_example, c("c", "h", "n", "o", "p", "s"),
         c("12c", "1h", "14n", "16o", "31p", "32s"),
         skip_absent = TRUE)

# Test 1: Check if the function adds expected columns
test_that("calc_eval_params adds the correct columns", {

  result <- calc_eval_params(mfd_example, verbose = FALSE)

  # Check if the new columns are in the result
  expected_columns <- c("nm", "dbe", "oc", "hc", "nc", "sc", "dbe_o", "ai", "wf", "z", "kmd",
                        "nosc", "delg0_cox", "relint13c_calc", "int13c_calc", "relint34s_calc",
                        "int34s_calc", "snp_check", "nsp_type", "co_tot", "nsp_tot",
                        "n_occurrence_orig", "n_assignments_orig", "ppm_filt")

  # Check if expected columns are present in the result
  expect_true(all(expected_columns %in% colnames(result)))
})

# Test 2: Check for missing required columns (should throw an error)
test_that("calc_eval_params throws an error for missing columns", {
  mfd_invalid <- mfd_example[, !c("12C")]  # Remove 'c' column to simulate an error

  expect_error(calc_eval_params(mfd_invalid),
               "The following required columns are not in 'mfd': 12C")
})

# Test 3: Check for an empty data.table (should throw an error)
test_that("calc_eval_params warns and errors for empty data", {
  mfd_empty <- data.table()

  expect_warning(
    expect_error(
      calc_eval_params(mfd_empty),
      "The 'mfd' data.table is empty."
    ),
    "No element / isotope columns identified in 'mfd'"
  )
})


# Test 4: Check the result for a small valid dataset
test_that("calc_eval_params works correctly on a small valid dataset", {
  result <- calc_eval_params(mfd_example, verbose = FALSE)

  # Check that specific values are added correctly (you can check specific expected values or ranges)
  expect_true("nm" %in% colnames(result))   # Ensure 'nm' is present
  expect_true("dbe" %in% colnames(result))  # Ensure 'dbe' is present
  expect_equal(result$`12C`[1], 10)              # Ensure the original column values are retained
  expect_true(result$oc[1] > 0)             # Ensure that the element ratios are calculated
})

# Test 6: Check that columns are correctly computed (e.g., element ratio)
test_that("Element ratios are correctly calculated", {
  result <- calc_eval_params(mfd_example, verbose = FALSE)

  # Check the calculated element ratios (e.g., oc ratio for the first row)
  expected_oc <- mfd_example$`16O`[1] / mfd_example$`12C`[1]
  expect_equal(result$oc[1], round(expected_oc, 3))
})

