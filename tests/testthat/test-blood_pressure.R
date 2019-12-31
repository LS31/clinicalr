test_that("MAP for missing DBP or SBP is missing", {
  expect_equal(estimate_mean_arterial_pressure(NA, 80), NA_integer_)
  expect_equal(estimate_mean_arterial_pressure(120, NA), NA_integer_)
  expect_equal(estimate_mean_arterial_pressure(NA, NA), NA_integer_)
})
