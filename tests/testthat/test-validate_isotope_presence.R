test_that("validate_isotope_presence errors if required columns are missing", {
  dt <- data.table::data.table(file_id = 1L)

  expect_error(
    validate_isotope_presence(dt, elements = "C"),
    regexp = "must contain the following columns"
  )
})


test_that("validate_isotope_presence errors if elements are missing", {
  dt <- data.table::data.table(
    file_id = 1L,
    isotope_group_id = 1L,
    mf = "C2H4O",
    iso_role = "parent",
    iso_element = NA_character_
  )

  expect_error(
    validate_isotope_presence(dt),
    regexp = "Argument 'elements' must contain at least one element symbol"
  )
})


test_that("validate_isotope_presence returns empty table for empty matching subset", {
  dt <- data.table::data.table(
    file_id = 1L,
    isotope_group_id = 1L,
    mf = "C2H4O",
    iso_role = "daughter",
    iso_element = "Cl"
  )

  res <- validate_isotope_presence(
    dt,
    elements = c("C", "N", "S")
  )

  expect_s3_class(res, "data.table")
  expect_equal(nrow(res), 0)
  expect_true(all(c(
    "file_id",
    "isotope_group_id",
    "mf",
    "parent_found",
    "n_elements_requested",
    "n_isotopes_expected",
    "n_isotopes_found",
    "isotopes_expected",
    "isotopes_found",
    "isotope_validation"
  ) %in% names(res)))
})


test_that("validated_all is assigned when all expected requested elements are found", {
  dt <- data.table::data.table(
    file_id = c(1L, 1L, 1L),
    isotope_group_id = c(1L, 1L, 1L),
    mf = c("C2H4NS", "C2H4NS", "C2H4NS"),
    iso_role = c("parent", "daughter", "daughter"),
    iso_element = c(NA_character_, "C", "S")
  )

  res <- validate_isotope_presence(
    dt,
    elements = c("C", "S"),
    require_all = TRUE
  )

  expect_equal(nrow(res), 1)
  expect_equal(res$parent_found, TRUE)
  expect_equal(res$n_elements_requested, 2L)
  expect_equal(res$n_isotopes_expected, 2L)
  expect_equal(res$n_isotopes_found, 2L)
  expect_equal(res$isotopes_expected, "C,S")
  expect_equal(res$isotopes_found, "C,S")
  expect_equal(res$isotope_validation, "validated_all")
})


test_that("validated_partial is assigned when only some expected elements are found", {
  dt <- data.table::data.table(
    file_id = c(1L, 1L),
    isotope_group_id = c(1L, 1L),
    mf = c("C2H4NS", "C2H4NS"),
    iso_role = c("parent", "daughter"),
    iso_element = c(NA_character_, "C")
  )

  dt2 <- data.table::rbindlist(list(
    dt,
    data.table::data.table(
      file_id = 2L,
      isotope_group_id = 1L,
      mf = "C2H4NS",
      iso_role = "daughter",
      iso_element = "S"
    )
  ))

  res <- validate_isotope_presence(
    dt2,
    elements = c("C", "S"),
    require_all = TRUE
  )

  row1 <- res[file_id == 1L & isotope_group_id == 1L]

  expect_equal(row1$parent_found, TRUE)
  expect_equal(row1$n_isotopes_expected, 2L)
  expect_equal(row1$n_isotopes_found, 1L)
  expect_equal(row1$isotopes_expected, "C,S")
  expect_equal(row1$isotopes_found, "C")
  expect_equal(row1$isotope_validation, "validated_partial")
})


test_that("parent_only is assigned when parent is found but no expected daughter is found", {
  dt <- data.table::data.table(
    file_id = c(1L, 2L, 2L),
    isotope_group_id = c(1L, 1L, 1L),
    mf = c("C2H4NS", "C2H4NS", "C2H4NS"),
    iso_role = c("parent", "daughter", "daughter"),
    iso_element = c(NA_character_, "C", "S")
  )

  res <- validate_isotope_presence(
    dt,
    elements = c("C", "S"),
    require_all = TRUE
  )

  row1 <- res[file_id == 1L & isotope_group_id == 1L]

  expect_equal(row1$parent_found, TRUE)
  expect_equal(row1$n_isotopes_expected, 2L)
  expect_equal(row1$n_isotopes_found, 0L)
  expect_equal(row1$isotopes_expected, "C,S")
  expect_equal(row1$isotopes_found, "")
  expect_equal(row1$isotope_validation, "parent_only")
})


test_that("daughter_only is assigned when daughter is found without parent", {
  dt <- data.table::data.table(
    file_id = c(1L, 2L, 2L),
    isotope_group_id = c(1L, 1L, 1L),
    mf = c("C2H4NS", "C2H4NS", "C2H4NS"),
    iso_role = c("parent", "daughter", "daughter"),
    iso_element = c(NA_character_, "C", "S")
  )

  res <- validate_isotope_presence(
    dt,
    elements = c("C", "S"),
    require_all = TRUE
  )

  row2 <- res[file_id == 2L & isotope_group_id == 1L]

  expect_equal(row2$parent_found, FALSE)
  expect_equal(row2$n_isotopes_expected, 2L)
  expect_equal(row2$n_isotopes_found, 2L)
  expect_equal(row2$isotopes_expected, "C,S")
  expect_equal(row2$isotopes_found, "C,S")
  expect_equal(row2$isotope_validation, "daughter_only")
})


test_that("formula with only C expected can still become validated_all", {
  dt <- data.table::data.table(
    file_id = c(1L, 1L),
    isotope_group_id = c(1L, 1L),
    mf = c("C2H4O", "C2H4O"),
    iso_role = c("parent", "daughter"),
    iso_element = c(NA_character_, "C")
  )

  res <- validate_isotope_presence(
    dt,
    elements = c("C", "N", "S"),
    require_all = TRUE
  )

  expect_equal(res$n_elements_requested, 3L)
  expect_equal(res$n_isotopes_expected, 1L)
  expect_equal(res$n_isotopes_found, 1L)
  expect_equal(res$isotopes_expected, "C")
  expect_equal(res$isotopes_found, "C")
  expect_equal(res$isotope_validation, "validated_all")
})


test_that("require_all FALSE returns validated_partial when at least one expected daughter is found", {
  dt <- data.table::data.table(
    file_id = c(1L, 1L, 2L),
    isotope_group_id = c(1L, 1L, 1L),
    mf = c("C2H4NS", "C2H4NS", "C2H4NS"),
    iso_role = c("parent", "daughter", "daughter"),
    iso_element = c(NA_character_, "C", "S")
  )

  res <- validate_isotope_presence(
    dt,
    elements = c("C", "S"),
    require_all = FALSE
  )

  row1 <- res[file_id == 1L & isotope_group_id == 1L]

  expect_equal(row1$n_isotopes_expected, 2L)
  expect_equal(row1$n_isotopes_found, 1L)
  expect_equal(row1$isotope_validation, "validated_partial")
})


test_that("elements not requested are ignored", {
  dt <- data.table::data.table(
    file_id = c(1L, 1L, 1L),
    isotope_group_id = c(1L, 1L, 1L),
    mf = c("C2H4NS", "C2H4NS", "C2H4NS"),
    iso_role = c("parent", "daughter", "daughter"),
    iso_element = c(NA_character_, "C", "Cl")
  )

  res <- validate_isotope_presence(
    dt,
    elements = c("C", "S"),
    require_all = TRUE
  )

  expect_equal(res$n_isotopes_expected, 1L)
  expect_equal(res$n_isotopes_found, 1L)
  expect_equal(res$isotopes_expected, "C")
  expect_equal(res$isotopes_found, "C")
  expect_equal(res$isotope_validation, "validated_all")
})


test_that("column order is correct", {
  dt <- data.table::data.table(
    file_id = c(1L, 1L),
    isotope_group_id = c(1L, 1L),
    mf = c("C2H4O", "C2H4O"),
    iso_role = c("parent", "daughter"),
    iso_element = c(NA_character_, "C")
  )

  res <- validate_isotope_presence(
    dt,
    elements = "C"
  )

  expect_equal(
    names(res),
    c(
      "file_id",
      "isotope_group_id",
      "mf",
      "parent_found",
      "n_elements_requested",
      "n_isotopes_expected",
      "n_isotopes_found",
      "isotopes_expected",
      "isotopes_found",
      "isotope_validation"
    )
  )
})


test_that("integration with create_isotope_expanded_table produces expected validation structure", {
  target_tbl <- create_isotope_expanded_table(
    mfd = ume::mf_data_demo[1:10],
    allow_duplicates = FALSE,
    elements = c("C", "N", "S")
  )

  res <- data.table::copy(target_tbl)
  res[, file_id := 1L]
  data.table::setcolorder(
    res,
    c("file_id", setdiff(names(res), "file_id"))
  )

  val <- validate_isotope_presence(
    dt_target_results = res,
    elements = c("C", "N", "S"),
    require_all = TRUE
  )

  expect_s3_class(val, "data.table")
  expect_true(nrow(val) > 0)
  expect_true(all(val$parent_found))
  expect_true(all(val$isotope_validation == "validated_all"))
})
