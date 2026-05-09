test_that("as_peaklist handles peaklist_demo correctly", {
  data("peaklist_demo")
  skip_if_not(exists("peaklist_demo"),
              message = "peaklist_demo dataset is missing")

  pl <- as_peaklist(peaklist_demo)

  # Basic structure checks
  expect_s3_class(pl, "data.table")
  expect_true(all(c("file_id", "mz", "i_magnitude", "peak_id") %in% names(pl)))

  # No NA in required columns
  expect_false(any(is.na(pl$mz)))
  expect_false(any(is.na(pl$i_magnitude)))

  # peak_id is consecutive integers
  #expect_equal(pl$peak_id, seq_len(nrow(pl)))
})


test_that("as_peaklist handles data.frame input", {
  df <- as.data.frame(peaklist_demo)
  pl <- as_peaklist(df)

  expect_s3_class(pl, "data.table")
  expect_true(all(c("mz", "i_magnitude") %in% names(pl)))
})


test_that("as_peaklist accepts numeric m/z vector", {
  mz <- c(100.1, 200.2, 300.3)
  pl <- as_peaklist(mz)

  expect_s3_class(pl, "data.table")
  expect_equal(nrow(pl), 3)
  expect_true(all(pl$file_id == 1L))
  expect_true(all(pl$i_magnitude == 1L))
  expect_equal(pl$mz, mz)
})


test_that("as_peaklist normalizes alias column names", {

  df <- data.frame(
    "m/z" = c(100, 200),
    "intensity" = c(10, 20),
    stringsAsFactors = FALSE
  )

  pl <- as_peaklist(df)

  # Rename happened?
  expect_true("mz" %in% names(pl))
  expect_true("i_magnitude" %in% names(pl))

  # Attribute exists and contains the expected savepoint
  #attributes(pl)
  orig <- attr(pl, "original_colnames")

  # Check structure rather than relying on names()
  expect_true(is.list(orig))
  expect_true("mz" %in% names(orig))
  expect_equal(orig[["mz"]], "m.z")

  expect_true("i_magnitude" %in% names(orig))
  expect_equal(orig[["i_magnitude"]], "intensity")
})


test_that("as_peaklist can read from CSV file", {
  tmp <- tempfile(fileext = ".csv")
  data.table::fwrite(peaklist_demo, tmp)

  pl <- as_peaklist(tmp)
  expect_s3_class(pl, "data.table")
  expect_equal(nrow(pl), nrow(peaklist_demo))
})


test_that("as_peaklist removes negative or NA mz/i_magnitude values", {
  bad <- data.table::data.table(
    mz = c(100, -5, NA),
    i_magnitude = c(10, NA, 5)
  )

  pl <- as_peaklist(bad)

  # Only the valid first row should survive
  expect_equal(nrow(pl), 1)
  expect_equal(pl$mz, 100)
  expect_equal(pl$i_magnitude, 10)
})


test_that("as_peaklist automatically assigns file_id when missing", {
  df <- data.frame(
    mz = c(100, 200),
    i_magnitude = c(10, 20)
  )

  pl <- as_peaklist(df)

  expect_true("file_id" %in% names(pl))
  expect_true(all(pl$file_id == 1L))
})


test_that("as_peaklist sets data.table key correctly", {
  pl <- as_peaklist(peaklist_demo)
  keycols <- data.table::key(pl)

  expect_identical(keycols, c("file_id", "mz"))
})

test_that("as_peaklist() generates file_id based on 'file' column", {

  pl <- data.table(
    file        = c("A.raw", "A.raw", "B.raw"),
    mz          = c(100, 200, 150),
    i_magnitude = c(10, 20, 30)
  )

  out <- as_peaklist(pl)

  expect_true("file_id" %in% names(out))
  expect_equal(sort(unique(out$file_id)), c(1L, 2L))
  expect_true(all(out$file_id[1:2] == out$file_id[1]))
  expect_true(out$file_id[3] != out$file_id[1])
})


test_that("as_peaklist normalizes alias column names", {

  pl <- data.table(
    `m/z`      = c(100, 200),
    intensity  = c(50, 80)
  )

  out <- as_peaklist(pl)

  # 1. Column names normalized
  expect_true("mz" %in% names(out))
  expect_true("i_magnitude" %in% names(out))

  # 2. Values are preserved
  expect_equal(out$mz, c(100, 200))
  expect_equal(out$i_magnitude, c(50, 80))
})

