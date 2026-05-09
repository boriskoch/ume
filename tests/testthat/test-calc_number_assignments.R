
# Sample data for testing
ms_ids <- c("file1", "file1", "file2", "file2", "file3")
peak_ids <- c(2, 2, 2, 2, 1)
mfs <- c("C10H10N2O8", "C10H12N2O8", "C10H10N2O8", "C10H11NOS4", "C10H24N4O2S")

# Test for correct output
test_that("calc_number_assignment returns correct counts", {
  expected_counts <- c(2, 2, 2, 2, 1)  # Expected counts for each entry
  result <- calc_number_assignment(ms_id = ms_ids, peak_id = peak_ids, mf = mfs)

  expect_equal(result, expected_counts)
})

# Test for error handling on unequal lengths
test_that("calc_number_assignment throws error for unequal lengths", {
  expect_error(calc_number_assignment(ms_id = ms_ids, peak_id = peak_ids, mf = c("C10H10N2O8")),
               "must all be the same and greater than 0")
  expect_error(calc_number_assignment(ms_id = ms_ids, peak_id = c(1, 2), mf = mfs),
               "must all be the same and greater than 0")
})

# Test for error handling on empty input
test_that("calc_number_assignment throws error for empty input", {
  expect_error(calc_number_assignment(ms_id = character(0), peak_id = character(0), mf = character(0)),
               "must all be the same and greater than 0")
})

# Test for the output being of integer type
test_that("calc_number_assignment returns integer vector", {
  result <- calc_number_assignment(ms_id = ms_ids, peak_id = peak_ids, mf = mfs)
  expect_type(result, "integer")
})

# Test for warning on duplicates
test_that("calc_number_assignment warns about duplicate entries", {
  mfs <- c("C10H10N2O8", "C10H10N2O8", "C10H10N2O8", "C10H11NOS4", "C10H24N4O2S")
  expect_warning(calc_number_assignment(ms_id = ms_ids, peak_id = peak_ids, mf = mfs),
                 "duplicate assignments for some combinations")
})
