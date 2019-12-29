context("QTc")

test_that("QTc for missing QT or heart rate is missing", {
  expect_equal(calculate_qtc_bazett(NA, 100), NA_integer_)
  expect_equal(calculate_qtc_bazett(400, NA), NA_integer_)
  expect_equal(calculate_qtc_bazett(NA, NA), NA_integer_)
  expect_equal(calculate_qtc_fridericia(NA, 100), NA_integer_)
  expect_equal(calculate_qtc_fridericia(400, NA), NA_integer_)
  expect_equal(calculate_qtc_fridericia(NA, NA), NA_integer_)
})

test_that("QTc crash on illegal arguments", {
  expect_error(calculate_qtc_bazett("A", 100))
  expect_error(calculate_qtc_bazett(400, "A"))
  expect_error(calculate_bsa_du_bois("A", 100))
  expect_error(calculate_bsa_du_bois(400, "A"))
})

test_that("QTc only for existing formulae", {
  expect_error(calculate_qtc(400, 100, "YOLO"))
})
