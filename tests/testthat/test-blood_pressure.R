context("Blood pressure")

test_that("MAP for missing dbp or sbp is missing", {
  expect_equal(calculate_map(NA, 80), NA_integer_)
  expect_equal(calculate_map(120, NA), NA_integer_)
  expect_equal(calculate_map(NA, NA), NA_integer_)
})
