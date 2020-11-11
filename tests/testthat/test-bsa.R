context("BSA")

test_that("BSA for missing height or weight is missing", {
  expect_equal(estimate_bsa_monsteller(NA, 180), NA)
  expect_equal(estimate_bsa_monsteller(80, NA), NA)
  expect_equal(estimate_bsa_monsteller(NA, NA), NA)
  expect_equal(estimate_bsa_du_bois(NA, 180), NA)
  expect_equal(estimate_bsa_du_bois(80, NA), NA)
  expect_equal(estimate_bsa_du_bois(NA, NA), NA)
})

test_that("BSA crash on illegal arguments", {
  expect_error(estimate_bsa_monsteller("A", 180))
  expect_error(estimate_bsa_monsteller(80, "A"))
  expect_error(estimate_bsa_du_bois("A", 180))
  expect_error(estimate_bsa_du_bois(80, "A"))
})

test_that("BSA only for existing formulae", {
  expect_error(estimate_bsa(80, 180, "YOLO"))
})
