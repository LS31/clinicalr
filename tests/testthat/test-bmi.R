context("BMI and IBW")

test_that("BMI for missing height or weight is missing", {
  expect_equal(calculate_bmi(NA, 180), NA)
  expect_equal(calculate_bmi(80, NA), NA)
  expect_equal(calculate_bmi(NA, NA), NA)
})

test_that("IBW for missing height or sex is missing", {
  expect_equal(estimate_ibw(NA, TRUE), NA)
  expect_equal(estimate_ibw(180, NA), NA)
  expect_equal(estimate_ibw(NA, NA), NA)
})
