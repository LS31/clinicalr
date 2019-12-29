context("Electrolytes")

test_that("Albumin-corrected calcium for missing values is missing", {
  expect_equal(correct_calcium_for_albumin(3, NA), NA_integer_)
  expect_equal(correct_calcium_for_albumin(NA, 23), NA_integer_)
})

test_that("Albumin-corrected calcium crash on illegal arguments", {
  expect_error(correct_calcium_for_albumin("A", 23))
  expect_error(correct_calcium_for_albumin(3, "A"))
})
