context("BSA")

test_that("BSA for missing height or weight is missing", {
  expect_equal(calculate_bsa_monsteller(NA, 180), NA_integer_)
  expect_equal(calculate_bsa_monsteller(80, NA), NA_integer_)
  expect_equal(calculate_bsa_monsteller(NA, NA), NA_integer_)
  expect_equal(calculate_bsa_du_bois(NA, 180), NA_integer_)
  expect_equal(calculate_bsa_du_bois(80, NA), NA_integer_)
  expect_equal(calculate_bsa_du_bois(NA, NA), NA_integer_)
})

test_that("BSA crash on illegal arguments", {
  expect_error(calculate_bsa_monsteller("A", 180))
  expect_error(calculate_bsa_monsteller(80, "A"))
  expect_error(calculate_bsa_du_bois("A", 180))
  expect_error(calculate_bsa_du_bois(80, "A"))
})

test_that("BSA only for existing formulae", {
  expect_error(calculate_bsa(80, 180, "YOLO"))
})
