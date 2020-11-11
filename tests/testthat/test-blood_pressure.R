context("Blood pressure")

test_that("MAP for missing DBP or SBP is missing", {
  expect_equal(estimate_map(NA, 80), NA)
  expect_equal(estimate_map(120, NA), NA)
  expect_equal(estimate_map(NA, NA), NA)
})

test_that("MAP for wrong input of DBP or SBP gives error", {
  expect_error(estimate_map(TRUE, 80))
  expect_error(estimate_map("A", 80))
  expect_error(estimate_map(120, TRUE))
  expect_error(estimate_map(120, "A"))
})

test_that("Pulse pressure for missing DBP or SBP is missing", {
  expect_equal(calculate_pulse_pressure(NA, 80), NA)
  expect_equal(calculate_pulse_pressure(120, NA), NA)
  expect_equal(calculate_pulse_pressure(NA, NA), NA)
})

test_that("Pulse pressure for wrong input of DBP or SBP gives error", {
  expect_error(calculate_pulse_pressure(TRUE, 80))
  expect_error(calculate_pulse_pressure("A", 80))
  expect_error(calculate_pulse_pressure(120, TRUE))
  expect_error(calculate_pulse_pressure(120, "A"))
})
