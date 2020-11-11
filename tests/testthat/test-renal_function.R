context("Renal function")

test_that("Cockcroft-Gault for missing values gives missing", {
  expect_equal(estimate_gfr_cockcroft(NA, 80, TRUE, 80), NA)
  expect_equal(estimate_gfr_cockcroft(120, NA, TRUE, 80), NA)
  expect_equal(estimate_gfr_cockcroft(120, 80, NA, 80), NA)
  expect_equal(estimate_gfr_cockcroft(120, 80, TRUE, NA), NA)
})

test_that("Cockcroft-Gault for wrong value types gives error", {
  expect_error(estimate_gfr_cockcroft("A", 80, TRUE, 80))
  expect_error(estimate_gfr_cockcroft(120, "A", TRUE, 80))
  expect_error(estimate_gfr_cockcroft(120, 80, "A", 80))
  expect_error(estimate_gfr_cockcroft(120, 80, TRUE, "A"))
})

test_that("MDRD for missing values gives missing", {
  expect_equal(estimate_gfr_mdrd(NA, 20, TRUE, TRUE), NA)
  expect_equal(estimate_gfr_mdrd(120, NA, TRUE, TRUE), NA)
  expect_equal(estimate_gfr_mdrd(120, 20, NA, TRUE), NA)
  expect_equal(estimate_gfr_mdrd(120, 20, TRUE, NA), NA)
})

test_that("MDRD for wrong value types gives error", {
  expect_error(estimate_gfr_mdrd("A", 20, TRUE, TRUE))
  expect_error(estimate_gfr_mdrd(120, "A", TRUE, TRUE))
  expect_error(estimate_gfr_mdrd(120, 20, "A", TRUE))
  expect_error(estimate_gfr_mdrd(120, 20, TRUE, "A"))
})

test_that("Schwartz for missing values gives missing", {
  expect_equal(estimate_gfr_schwartz(NA, 120), NA)
  expect_equal(estimate_gfr_schwartz(120, NA), NA)
})

test_that("Schwartz for wrong value types gives error", {
  expect_error(estimate_gfr_schwartz("A", 120))
  expect_error(estimate_gfr_schwartz(120, "A"))
})

test_that("CKD-EPI for missing values gives missing", {
  expect_equal(estimate_gfr_ckdepi(NA, 20, TRUE, TRUE), NA)
  expect_equal(estimate_gfr_ckdepi(120, NA, TRUE, TRUE), NA)
  expect_equal(estimate_gfr_ckdepi(120, 20, NA, TRUE), NA)
  expect_equal(estimate_gfr_ckdepi(120, 20, TRUE, NA), NA)
})

test_that("CKD-EPI for wrong value types gives error", {
  expect_error(estimate_gfr_ckdepi("A", 20, TRUE, TRUE))
  expect_error(estimate_gfr_ckdepi(120, "A", TRUE, TRUE))
  expect_error(estimate_gfr_ckdepi(120, 20, "A", TRUE))
  expect_error(estimate_gfr_ckdepi(120, 20, TRUE, "A"))
})
