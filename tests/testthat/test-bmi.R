context("BMI")

test_that("BMI for missing height or weight is missing", {
  expect_equal(calculate_bmi(NA, 180), NA_integer_)
  expect_equal(calculate_bmi(80, NA), NA_integer_)
  expect_equal(calculate_bmi(NA, NA), NA_integer_)
})
