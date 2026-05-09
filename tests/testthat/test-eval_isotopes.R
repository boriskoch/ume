test_that("eval_isotopes reduces rows correctly when remove_isotopes = TRUE", {

    mfd <- suppressWarnings(ume_assign_formulas(
      pl = peaklist_demo,
      formula_library = lib_demo,
      verbose = FALSE,
      ma_dev = 0.5,
      pol = "neg"
    ))

  # Apply isotope evaluation
    mfd <- suppressMessages(eval_isotopes(
      mfd = mfd,
      remove_isotopes = TRUE,
      verbose = FALSE
    ))

  # Core assertion: row count
  expect_equal(nrow(mfd), 9245)

})
