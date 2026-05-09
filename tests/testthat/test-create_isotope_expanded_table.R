test_that("create_isotope_expanded_table returns expected core columns", {
  res <- create_isotope_expanded_table(
    mfd = ume::mf_data_demo,
    allow_duplicates = FALSE,
    elements = c("C", "N", "S")
  )

  expect_s3_class(res, "data.table")

  expect_true(all(c(
    "isotope_group_id",
    "iso_role",
    "iso_element",
    "iso_from",
    "iso_to",
    "mf",
    "mf_iso",
    "nm",
    "mass"
  ) %in% names(res)))

  expect_true(nrow(res) > 0)
})


test_that("parent formulas are always retained", {
  res <- create_isotope_expanded_table(
    mfd = ume::mf_data_demo,
    allow_duplicates = FALSE,
    elements = c("C", "N", "S")
  )

  parent_mf <- unique(ume::mf_data_demo$mf)
  expect_true(all(parent_mf %in% res$mf))
})


test_that("allow_duplicates = TRUE keeps id column when available", {
  dt <- data.table::copy(ume::mf_data_demo)

  expect_true("peak_id" %in% names(dt))

  res_dup <- create_isotope_expanded_table(
    mfd = dt,
    id_col = "peak_id",
    allow_duplicates = TRUE,
    elements = c("C", "N", "S")
  )

  res_unique <- create_isotope_expanded_table(
    mfd = dt,
    id_col = "peak_id",
    allow_duplicates = FALSE,
    elements = c("C", "N", "S")
  )

  expect_true("peak_id" %in% names(res_dup))
  expect_false("peak_id" %in% names(res_unique))
  expect_true(nrow(res_dup) >= nrow(res_unique))
})


test_that("missing id_col triggers fallback message and unique-only behavior", {
  dt <- data.table::copy(ume::mf_data_demo)
  if ("peak_id" %in% names(dt)) dt[, peak_id := NULL]

  expect_message(
    res <- create_isotope_expanded_table(
      mfd = dt,
      id_col = "peak_id",
      allow_duplicates = TRUE,
      elements = c("C", "N", "S")
    ),
    regexp = "was not found in 'mfd'.*unique isotope compositions only"
  )

  expect_false("peak_id" %in% names(res))
  expect_true("isotope_group_id" %in% names(res))
})


test_that("elements restrict isotope expansion", {
  res_c <- create_isotope_expanded_table(
    mfd = ume::mf_data_demo,
    allow_duplicates = FALSE,
    elements = "C"
  )

  res_cns <- create_isotope_expanded_table(
    mfd = ume::mf_data_demo,
    allow_duplicates = FALSE,
    elements = c("C", "N", "S")
  )

  expect_true(nrow(res_cns) >= nrow(res_c))

  if ("15N" %in% names(res_c)) {
    daughter_n <- res_c[iso_role == "daughter" & iso_element == "N"]
    expect_true(nrow(daughter_n) == 0)
  }

  if ("34S" %in% names(res_c)) {
    daughter_s <- res_c[iso_role == "daughter" & iso_element == "S"]
    expect_true(nrow(daughter_s) == 0)
  }

  if ("13C" %in% names(res_c)) {
    daughter_c <- res_c[iso_role == "daughter" & iso_element == "C"]
    expect_true(nrow(daughter_c) > 0)
    expect_true(any(daughter_c$`13C` > 0))
  }
})


test_that("unknown elements throw an error", {
  expect_error(
    create_isotope_expanded_table(
      mfd = ume::mf_data_demo,
      elements = c("C", "Xx")
    ),
    regexp = "Unknown element\\(s\\) in 'elements'"
  )
})


test_that("isotope_group_id is present and non-missing", {
  res <- create_isotope_expanded_table(
    mfd = ume::mf_data_demo,
    allow_duplicates = FALSE,
    elements = c("C", "N", "S")
  )

  expect_true("isotope_group_id" %in% names(res))
  expect_false(any(is.na(res$isotope_group_id)))
})


test_that("isotope_group_id groups parent and daughter isotopologues", {
  res <- create_isotope_expanded_table(
    mfd = ume::mf_data_demo,
    allow_duplicates = FALSE,
    elements = c("C", "N", "S")
  )

  grp <- res[, .(
    n = .N,
    roles = paste(sort(unique(iso_role)), collapse = ",")
  ), by = isotope_group_id]

  expect_true(any(grp$n > 1))
  expect_true(any(grepl("parent", grp$roles) & grepl("daughter", grp$roles)))
})


test_that("column order starts with grouping and isotope annotation columns", {
  res <- create_isotope_expanded_table(
    mfd = ume::mf_data_demo,
    allow_duplicates = FALSE,
    elements = c("C", "N", "S")
  )

  expect_equal(
    names(res)[1:9],
    c(
      "isotope_group_id",
      "iso_role",
      "iso_element",
      "iso_from",
      "iso_to",
      "mf",
      "mf_iso",
      "nm",
      "mass"
    )
  )
})


test_that("parent rows are annotated correctly", {
  res <- create_isotope_expanded_table(
    mfd = ume::mf_data_demo,
    allow_duplicates = FALSE,
    elements = c("C", "N", "S")
  )

  parent_rows <- res[iso_role == "parent"]

  expect_true(nrow(parent_rows) > 0)
  expect_true(all(is.na(parent_rows$iso_element)))
  expect_true(all(is.na(parent_rows$iso_from)))
  expect_true(all(is.na(parent_rows$iso_to)))
})


test_that("daughter rows are annotated correctly", {
  res <- create_isotope_expanded_table(
    mfd = ume::mf_data_demo,
    allow_duplicates = FALSE,
    elements = c("C", "N", "S")
  )

  daughter_rows <- res[iso_role == "daughter"]

  expect_true(nrow(daughter_rows) > 0)
  expect_true(all(!is.na(daughter_rows$iso_element)))
  expect_true(all(!is.na(daughter_rows$iso_from)))
  expect_true(all(!is.na(daughter_rows$iso_to)))
})


test_that("toy example expands one parent formula correctly", {
  dt <- data.table::data.table(
    peak_id = 1L,
    `12C` = 2L,
    `1H` = 4L,
    `14N` = 1L,
    `16O` = 1L
  )

  res <- create_isotope_expanded_table(
    mfd = dt,
    id_col = "peak_id",
    allow_duplicates = TRUE,
    elements = c("C", "N")
  )

  expect_true("peak_id" %in% names(res))
  expect_true("isotope_group_id" %in% names(res))
  expect_true(all(res$isotope_group_id == 1L))

  # parent retained
  expect_true(any(res$iso_role == "parent" & res$`12C` == 2L & res$`13C` == 0L))

  # 13C daughter created
  expect_true(any(
    res$iso_role == "daughter" &
      res$iso_element == "C" &
      res$`12C` == 1L &
      res$`13C` == 1L
  ))

  # 15N daughter created if supported
  if ("15N" %in% names(res)) {
    expect_true(any(
      res$iso_role == "daughter" &
        res$iso_element == "N" &
        res$`14N` == 0L &
        res$`15N` == 1L
    ))
  }
})


test_that("allow_duplicates FALSE creates repeated isotope_group_id values across isotopologues", {
  res <- create_isotope_expanded_table(
    mfd = ume::mf_data_demo,
    allow_duplicates = FALSE,
    elements = c("C", "N", "S")
  )

  counts <- res[, .N, by = isotope_group_id]
  expect_true(any(counts$N > 1))
})


test_that("allow_duplicates TRUE preserves grouping column when available", {
  dt <- data.table::copy(ume::mf_data_demo)

  res <- create_isotope_expanded_table(
    mfd = dt,
    id_col = "peak_id",
    allow_duplicates = TRUE,
    elements = c("C")
  )

  expect_true("peak_id" %in% names(res))

  parent_peak_ids <- unique(res[iso_role == "parent", peak_id])
  expect_true(length(parent_peak_ids) > 0)
})


test_that("result contains parent and daughter rows when isotopes can be generated", {
  res <- create_isotope_expanded_table(
    mfd = ume::mf_data_demo,
    allow_duplicates = FALSE,
    elements = c("C", "N", "S")
  )

  expect_true(any(res$iso_role == "parent"))
  expect_true(any(res$iso_role == "daughter"))
})

