test_that("roll_outcome_count() works", {
  testthat::expect_snapshot(roll_outcome_count("3*2d6 + d20 + 1d10 - 1 +    4"))
  testthat::expect_snapshot(roll_outcome_count("40d6"))
})
