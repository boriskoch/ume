# pl <- peaklist_demo  # Modify this data to include peaks outside library range
# formula_library <- ume::lib_demo

# to do: This doesn't work correctly because mfd is returned invisible()
# Temporarily fixed with suppressWar

test_that("assign_formulas outputs warnings for peaks outside library mass range", {
  expect_warning(assign_formulas(pl = peaklist_demo, formula_library = lib_demo[mass %between% c(250,251)],
                                 pol = "neg", ma_dev = 0.2, verbose = FALSE),
                 "Mass range in peaklist is not completely covered by formula library")
})

# Run assignment (we expect a warning about truncated peaks)
test_that("assign_formulas produces expected columns in the output", {
  result <- suppressWarnings(assign_formulas(pl = peaklist_demo, formula_library = lib_demo,
                                             ma_dev = 0.5, pol = "neg", verbose = FALSE))  |> suppressMessages()

  # Test Correct Output Columns
  # Check if the expected columns are present in the result
  expected_columns <- c("file_id", "mf", "m_cal", "13C", "15N", "34S", "del", "ppm", "mf_id")
  expect_true(all(expected_columns %in% names(result)))
})

test_that("assign_formulas returns expected row count for demo data", {
  testthat::local_edition(3)
  # skip_on_cran()  # avoid long runs on CRAN if this is heavy

# Use the demo inputs shipped with ume
  mfd <- assign_formulas(
      pl =  peaklist_demo,
      formula_library = lib_demo,
      verbose = FALSE,
      ma_dev = 0.5,
      pol = "neg"
    )

  # Basic structure sanity checks
  expect_s3_class(mfd, "data.table")
  expect_true(nrow(mfd) > 0)

  # Core assertion: row count
  expect_equal(nrow(mfd), 27500)
})

test_that("assign_formulas returns expected row count for single mass vector", {
  mfd <- assign_formulas(pl = c(297.01241, 213.00124), pol = "pos", ma_dev = 1,
                         formula_library = lib_demo)  |> suppressMessages()
  # Core assertion: row count
  expect_equal(nrow(mfd), 9)
})

