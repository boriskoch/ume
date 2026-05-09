# test-f_label.R
# Unit tests for internal helper function .f_label()

test_that(".f_label correctly matches isotope-specific patterns", {
  lookup <- data.table::data.table(
    name_substitute = c("Number of C", "Number of H", "Number of N",
                        "Number of S", "Number of P", "Number of O"),
    name_pattern    = c("^12C$", "^1H$", "^14N$", "^32S$", "^31P$", "^16O$")
  )

  expect_equal(.f_label("12C", lookup), "Number of C")
  expect_equal(.f_label("1H",  lookup), "Number of H")
  expect_equal(.f_label("14N", lookup), "Number of N")
  expect_equal(.f_label("32S", lookup), "Number of S")
  expect_equal(.f_label("31P", lookup), "Number of P")
  expect_equal(.f_label("16O", lookup), "Number of O")
})

test_that(".f_label returns 'Normalized intensity' for norm_int", {
  lookup <- data.table::data.table(
    name_substitute = "Normalized intensity",
    name_pattern    = "^norm_int$"
  )
  expect_equal(.f_label("norm_int", lookup), "Normalized intensity")
})

test_that(".f_label returns original name when no pattern matches", {
  lookup <- data.table::data.table(
    name_substitute = "Number of C",
    name_pattern    = "^12C$"
  )

  expect_equal(.f_label("unknown_var", lookup), "unknown_var")
})

test_that(".f_label returns first match if multiple patterns match", {
  lookup <- data.table::data.table(
    name_substitute = c("Label 1", "Label 2"),
    name_pattern    = c("^x$", "^x$")
  )

  # Should return first match only
  expect_equal(.f_label("x", lookup), "Label 1")
})

test_that(".f_label warns and returns original name for malformed lookup", {
  malformed_lookup <- data.table::data.table(
    wrong_col1 = "Something",
    wrong_col2 = ".*"
  )

  expect_warning(
    result <- .f_label("12C", malformed_lookup)
  )
  expect_equal(result, "12C")
})

test_that(".f_label handles NULL lookup gracefully", {
  expect_warning(r <- .f_label("12C", NULL))
  expect_equal(r, "12C")
})

test_that(".f_label handles NA safely", {
  lookup <- data.table::data.table(
    name_substitute = "Number of C",
    name_pattern    = "^12C$"
  )

  expect_equal(.f_label(NA_character_, lookup), NA_character_)
})
