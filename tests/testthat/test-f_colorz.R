# ======================================================================
# test-f_colorz.R
# Unit tests for f_colorz() and f_colpal_selection()
# ======================================================================

test_that("f_colorz() rejects non-numeric z", {
  expect_error(f_colorz(z = "a"), "not numeric")
  expect_error(f_colorz(z = list(1,2)), "not numeric")
})

test_that("f_colorz() returns correct length", {
  z <- c(1, 5, 10)
  cols <- f_colorz(z, palname = "viridis", col_num = 50)
  expect_length(cols, length(z))
})

test_that("f_colorz() handles constant z vectors", {
  z <- rep(42, 10)
  cols <- f_colorz(z, palname = "viridis")
  expect_true(length(unique(cols)) == 1)
})

test_that("f_colorz() applies log transform if requested", {
  z <- c(1, 5, 10)
  cols1 <- f_colorz(z, palname = "viridis", tf = FALSE)
  cols2 <- f_colorz(z, palname = "viridis", tf = TRUE)

  # They should NOT be identical
  expect_false(identical(cols1, cols2))
})

test_that("f_colorz() rejects log transform with non-positive values", {
  expect_error(
    f_colorz(c(-1, 3), tf = TRUE),
    "requires values > 0"
  )
})

test_that("f_colorz() errors on unknown palette", {
  expect_error(
    f_colorz(z = 1:10, palname = "not_a_palette"),
    "Unknown palette"
  )
})

test_that("f_colorz() index mapping covers full palette range", {
  z <- seq(0, 1, length.out = 1000)
  col_num <- 100
  cols <- f_colorz(z, palname = "viridis", col_num = col_num)

  # Extract palette used inside function
  pal <- .palette_builders$viridis(col_num)

  # For continuous z, both first and last palette colors should appear
  expect_true(pal[1] %in% cols)
  expect_true(pal[col_num] %in% cols)
})

test_that("f_colpal_selection() returns expected structure", {
  res <- f_colpal_selection("awi")

  expect_type(res, "list")
  expect_named(res, c("cpal", "paltype", "colsel"))

  expect_length(res$cpal, 40)
  expect_true(res$paltype %in% c("square", "limited"))
  expect_true(is.character(res$colsel))
})

test_that("f_colpal_selection() errors on unknown palette", {
  expect_error(
    f_colpal_selection("unknown_palette"),
    "Unknown palette"
  )
})

test_that("All palettes in registry can be generated", {
  for (nm in names(.palette_builders)) {
    builder <- .palette_builders[[nm]]

    expect_type(builder, "closure")

    pal <- builder(50)

    expect_length(pal, 50)
    expect_type(pal, "character")
    expect_false(any(is.na(pal)))
  }
})


