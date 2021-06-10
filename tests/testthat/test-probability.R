test_that("roll_outcome_count() works", {

  wide <- roll_outcome_count("3*2d6 + d20 + 1d10 - 1 +    4")
  long <- roll_outcome_count("40d6")

  expect_equal(nrow(wide), 59)
  expect_equal(nrow(long), 201)

  expect_equal(wide$count[30], 280)
  expect_equal(min(wide$outcome), 11)
  expect_equal(max(wide$outcome), 69)

  expect_equal(long$count[1], 1)
  expect_equal(long$count[201], 1)
  expect_equal(min(long$outcome), 40)
  expect_equal(max(long$outcome), 240)
})
