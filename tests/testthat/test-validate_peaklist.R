# test-validate_peaklist.R
# ------------------------------------------------------------------------------

test_that("validate_peaklist() accepts a valid peaklist", {

  pl <- data.table::data.table(
    file_id     = c(1L, 1L, 2L),
    file        = c("A.raw", "A.raw", "B.raw"),
    peak_id     = c(1L, 2L, 1L),
    mz          = c(100.1, 200.2, 150.3),
    i_magnitude = c(1000, 5000, 2000),
    s_n         = c(10, 20, 15),
    res         = c(50000, 60000, 55000)
  )

  expect_silent(validate_peaklist(pl))
  expect_identical(validate_peaklist(pl), pl)
})


test_that("validate_peaklist() rejects non-data.table input", {

  df <- data.frame(
    file_id = 1:3,
    peak_id = 1:3,
    mz = c(100, 200, 300),
    i_magnitude = c(10, 20, 30)
  )

  expect_error(
    validate_peaklist(df),
    "data.table"
  )
})


test_that("validate_peaklist() detects missing required columns", {

  pl_missing <- data.table::data.table(
    file_id     = 1:3,
    peak_id     = 1:3,
    mz          = c(100, 200, 300)
    # missing i_magnitude
  )

  expect_error(
    validate_peaklist(pl_missing),
    "missing required columns"
  )
})


test_that("validate_peaklist() detects wrong mz type", {

  pl_bad_mz <- data.table::data.table(
    file_id     = 1:3,
    peak_id     = 1:3,
    mz          = c("100", "200", "300"),  # wrong type
    i_magnitude = c(1, 2, 3)
  )

  expect_error(validate_peaklist(pl_bad_mz), "mz")
})


test_that("validate_peaklist() detects wrong file_id type", {

  pl_bad_file_id <- data.table::data.table(
    file_id     = c("A", "A", "B"),    # wrong type
    peak_id     = 1:3,
    mz          = c(100, 200, 300),
    i_magnitude = c(1, 2, 3)
  )

  expect_error(validate_peaklist(pl_bad_file_id), "file_id")
})



test_that("validate_peaklist() detects duplicated (file_id, peak_id)", {

  pl_dup <- data.table::data.table(
    file_id     = c(1L, 1L),
    peak_id     = c(1L, 1L),
    mz          = c(100, 150),
    i_magnitude = c(1000, 2000)
  )

  expect_error(
    validate_peaklist(pl_dup),
    "non-unique"
  )
})


test_that("validate_peaklist() rejects negative m/z values", {

  pl_neg <- data.table::data.table(
    file_id     = 1L,
    peak_id     = 1L,
    mz          = -5,
    i_magnitude = 1000
  )

  expect_error(
    validate_peaklist(pl_neg),
    "negative"
  )
})


test_that("validate_peaklist() allows optional columns to be missing", {

  pl_minimal <- data.table::data.table(
    file_id     = 1L,
    peak_id     = 1L,
    mz          = 100.1,
    i_magnitude = 2000
    # s_n and res are optional
  )

  expect_silent(validate_peaklist(pl_minimal))
})


test_that("validate_peaklist() keeps attributes intact", {

  pl <- data.table::data.table(
    file_id     = 1L,
    peak_id     = 1L,
    mz          = 100,
    i_magnitude = 1000
  )

  data.table::setattr(pl, "test_attr", "XYZ")

  out <- validate_peaklist(pl)

  expect_equal(attr(out, "test_attr"), "XYZ")
})
