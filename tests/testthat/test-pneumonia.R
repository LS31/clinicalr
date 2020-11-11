context("Pneumonia")

test_that("CURB65 for missing values gives missing", {
  expect_equal(score_curb65(NA, 10, 10, 10, 10, 10), NA)
  expect_equal(score_curb65(TRUE, NA, 10, 10, 10, 10), NA)
  expect_equal(score_curb65(TRUE, 10, NA, 10, 10, 10), NA)
  expect_equal(score_curb65(TRUE, 10, 10, NA, 10, 10), NA)
  expect_equal(score_curb65(TRUE, 10, 10, 10, NA, 10), NA)
  expect_equal(score_curb65(TRUE, 10, 10, 10, 10, NA), NA)
})

test_that("CURB65 for wrong values gives error (no guesses)", {
  expect_error(score_curb65("A", 10, 10, 10, 10, 10))
  expect_error(score_curb65(TRUE, "A", 10, 10, 10, 10))
  expect_error(score_curb65(TRUE, 10, "A", 10, 10, 10))
  expect_error(score_curb65(TRUE, 10, 10, "A", 10, 10))
  expect_error(score_curb65(TRUE, 10, 10, 10, "A", 10))
  expect_error(score_curb65(TRUE, 10, 10, 10, 10, "A"))
})
