test_that("Test the entire UME data pipeline", {
  # testthat::local_edition(3)
  # skip_on_cran()  # avoid long runs on CRAN if this is heavy

    mfd <- suppressWarnings(assign_formulas(
      pl = peaklist_demo,
      formula_library = lib_demo,
      verbose = FALSE,
      ma_dev = 0.5,
      pol = "neg"
    ))

  # Basic structure sanity checks
  expect_s3_class(mfd, "data.table")
  expect_true(nrow(mfd) > 0)
  expect_equal(nrow(mfd), 27500)  # Core assertion: row count

  # Apply isotope evaluation
  mfd <- suppressMessages(eval_isotopes(mfd = mfd, remove_isotopes = TRUE, verbose = FALSE))
  expect_equal(nrow(mfd), 9245)   # Core assertion: row count

  mfd <- add_known_mf(mfd = mfd)

  expect_equal(nrow(mfd[categories %like% "ideg_pos"]), 12)

  mfd_filt <- suppressWarnings(remove_blanks(
    mfd = mfd,
    blank_file_ids = c(1),
    blank_prevalence = 0.5,
    verbose = F
  ))
  expect_equal(nrow(mfd_filt), 9073)

  mfd_filt <- calc_eval_params(mfd = mfd_filt)
  expect_equal(mfd_filt[, round(mean(dbe), 2)], 6.27)

  ds <- calc_data_summary(mfd_filt)
  expect_equal(nrow(ds), 12)
  expect_equal(ds[, round(mean(`wa(mz)`), 3)], 275.083)

  mfd_filt <- calc_norm_int(mfd = mfd_filt, normalization = "sum_ubiq")
  expect_equal(mfd_filt[, round(max(norm_int), 2)], 7.17)

  mfd_filt <- calc_norm_int(mfd = mfd_filt, normalization = "bp")
  expect_equal(mfd_filt[, max(norm_int)], 100)

  mfd_filt <- filter_int(mfd = mfd_filt, norm_int_min = 0.5)
  expect_equal(mfd_filt[, .N], 8785)

  mfd_filt$n_assignments

})

