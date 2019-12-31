test_that("MAP for missing DBP or SBP is missing", {
  expect_equal(estimate_map(NA, 80), NA_integer_)
  expect_equal(estimate_map(120, NA), NA_integer_)
  expect_equal(estimate_map(NA, NA), NA_integer_)
})
