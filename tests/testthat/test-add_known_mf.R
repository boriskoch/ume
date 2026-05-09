test_that("add_known_mf() works with vector input", {
  skip_if_not_installed("ume")

  mf_vec <- c("C6H12O6", "C10H16O4")

  out <- add_known_mf(mf_vec)

  # Basic structure
  expect_s3_class(out, "data.table")
  expect_true(all(c("mf", "categories") %in% names(out)))

  # Output size
  expect_equal(nrow(out), length(mf_vec))

  # MFs preserved
  expect_equal(out$mf, mf_vec)
})


test_that("add_known_mf() works with data.table input", {
  skip_if_not_installed("ume")

  dt <- data.table::data.table(
    mf = c("C6H12O6", "C10H16O4"),
    intensity = c(10, 20)
  )

  out <- add_known_mf(dt)

  # Structure
  expect_s3_class(out, "data.table")
  expect_true(all(colnames(dt) %in% names(out)))
  expect_true("categories" %in% names(out))

  # Values preserved
  expect_equal(out$intensity, dt$intensity)
  expect_equal(out$mf, dt$mf)
})


test_that("add_known_mf() detects missing mf_col in table input", {
  dt <- data.table::data.table(x = 1:3)

  expect_error(
    add_known_mf(dt, mf_col = "mf"),
    "Column 'mf' not found"
  )
})


test_that("add_known_mf(wide = TRUE) returns indicator columns", {
  skip_if_not_installed("ume")

  dt <- data.table::data.table(
    mf = unique(ume::known_mf$mf)[1:5]
  )

  out <- add_known_mf(dt, wide = TRUE)

  # Should contain MF + categories + several wide columns
  expect_true("mf" %in% names(out))
  expect_true("categories" %in% names(out))

  # Known categories present?
  categ <- ume::tab_ume_labels[use_in_ume == 1, unique(label)]

  # At least some indicator columns must exist
  expect_true(any(categ %in% names(out)))
})


test_that("surfactant column (if present) has no NAs", {
  skip_if_not_installed("ume")

  mf_vec <- unique(ume::known_mf$mf)[1:50]
  out <- add_known_mf(mf_vec, wide = TRUE)

  if ("surfactant" %in% names(out)) {
    # Expectation if the column exists
    expect_false(any(is.na(out$surfactant)))
  } else {
    # Expectation if the column does NOT exist
    succeed("surfactant column is not present for these formulas.")
  }
})

test_that("unknown formulas return NA for categories", {
  skip_if_not_installed("ume")

  mf_vec <- c("C999H999", "Xx1")  # definitely unknown
  out <- add_known_mf(mf_vec)

  expect_true(all(is.na(out$categories)))
})
