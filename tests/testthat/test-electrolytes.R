context("Electrolytes")

test_that("Albumin-corrected calcium for missing values is missing", {
  expect_equal(adjust_calcium_for_albumin(3, NA), NA_integer_)
  expect_equal(adjust_calcium_for_albumin(NA, 23), NA_integer_)
})

test_that("Albumin-corrected calcium crash on illegal arguments", {
  expect_error(adjust_calcium_for_albumin("A", 23))
  expect_error(adjust_calcium_for_albumin(3, "A"))
})

test_that("Glucose-corrected sodium for missing values is missing", {
  expect_equal(adjust_sodium_for_glucose(120, NA), NA_integer_)
  expect_equal(adjust_sodium_for_glucose(NA, 25), NA_integer_)
})

test_that("Glucose-corrected sodium crash on illegal arguments", {
  expect_error(adjust_sodium_for_glucose("A", 25))
  expect_error(adjust_sodium_for_glucose(120, "A"))
  expect_error(adjust_sodium_for_glucose(120, 25, "A"))
  expect_error(adjust_sodium_for_glucose(120, 25, NA))
})

