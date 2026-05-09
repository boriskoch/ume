
# Assuming the function add_missing_element_columns() is already defined

test_that("add_missing_element_columns adds missing columns with 0", {

  # Create a test data.table
  mfd <- data.table(
    id = 1:3,
    value = c(10, 20, 30)
  )

  # Add the missing "15n" column (it's missing)
  result <- add_missing_element_columns(mfd, missing_cols = "15n")

  # Check if the "15n" column has been added and is populated with 0
  expect_true("15n" %in% colnames(result))
  expect_equal(result$`15n`, c(0, 0, 0))

})

test_that("add_missing_element_columns does not modify existing columns", {

  # Create a test data.table with an existing "15n" column
  mfd <- data.table(
    id = 1:3,
    value = c(10, 20, 30),
    `15n` = c(1, 2, 3)  # Column already exists
  )

  # Add the missing "15n" column (it already exists, so no modification should happen)
  result <- add_missing_element_columns(mfd, missing_cols = "15n")

  # The "15n" column should not be overwritten
  expect_equal(result$`15n`, c(1, 2, 3))
})

test_that("add_missing_element_columns adds multiple missing columns", {

  # Create a test data.table with no isotope columns
  mfd <- data.table(
    id = 1:3,
    value = c(10, 20, 30)
  )

  # Add multiple missing isotope columns
  result <- add_missing_element_columns(mfd, missing_cols = c("15n", "na", "d"))

  # Check if the columns "15n", "na", and "d" have been added with 0 values
  expect_true("15n" %in% colnames(result))
  expect_true("na" %in% colnames(result))
  expect_true("d" %in% colnames(result))

  expect_equal(result$`15n`, c(0, 0, 0))
  expect_equal(result$na, c(0, 0, 0))
  expect_equal(result$d, c(0, 0, 0))
})

test_that("add_missing_element_columns does not add columns that already exist", {

  # Create a test data.table with some columns already present
  mfd <- data.table(
    id = 1:3,
    value = c(10, 20, 30),
    `15n` = c(1, 2, 3)
  )

  # Add missing columns, including the one that already exists ("15n")
  result <- add_missing_element_columns(mfd, missing_cols = c("15n", "na"))

  # "15n" should not be added again; "na" should be added
  expect_true("15n" %in% colnames(result))
  expect_true("na" %in% colnames(result))

  # Check that "15n" was not changed and "na" was added with 0 values
  expect_equal(result$`15n`, c(1, 2, 3))
  expect_equal(result$na, c(0, 0, 0))
})

