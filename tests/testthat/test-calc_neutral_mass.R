
test_that("calc_neutral_mass returns correct values for negative ionization", {
  result <- calc_neutral_mass(100, pol = "neg")
  expect_equal(result, 100 + 1.0072763)
})

test_that("calc_neutral_mass returns correct values for positive ionization", {
  result <- calc_neutral_mass(100, pol = "pos")
  expect_equal(result, 100 - 1.0072763)
})

test_that("calc_neutral_mass returns correct values for neutral masses", {
  result <- calc_neutral_mass(100, pol = "neutral")
  expect_equal(result, 100)
})

test_that("calc_neutral_mass throws an error when mz is non-positive", {
  expect_error(calc_neutral_mass(-1), "The mz values must be >0.")
  expect_error(calc_neutral_mass(0), "The mz values must be >0.")
})

test_that("calc_neutral_mass warns when mz is unusually high", {
  expect_warning(calc_neutral_mass(100001), "The mz values seem unusually high.")
})

test_that("calc_neutral_mass works for vector input", {
  result <- calc_neutral_mass(c(100, 200), pol = "neg")
  expect_equal(result, c(100 + 1.0072763, 200 + 1.0072763))
})
