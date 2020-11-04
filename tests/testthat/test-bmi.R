context("BMI and IBW")

test_that("BMI for missing height or weight is missing", {
  expect_equal(calculate_bmi(NA, 180), NA_integer_)
  expect_equal(calculate_bmi(80, NA), NA_integer_)
  expect_equal(calculate_bmi(NA, NA), NA_integer_)
})

test_that("IBW for missing height is missing", {
  expect_equal(estimate_ibw(NA, TRUE), NA_integer_)
})


test_that("IBW for missing sex raises error", {
  expect_error(estimate_ibw(180, "bla"))
  expect_error(estimate_ibw(180, NA))
  expect_error(estimate_ibw(NA, NA))
})
