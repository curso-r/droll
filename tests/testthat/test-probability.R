test_that("roll_outcome_count() works", {

  d6 <- Dice(1:6)
  d20 <- Dice(1:20)
  d10 <- Dice(1:10)

  wide <- roll_outcome_count(3* (2*d6) + d20 + 1*d10 - 1 +    4)

  expect_equal(nrow(wide), 59)
  expect_equal(wide$count[30], 280)
  expect_equal(min(wide$outcome), 11)
  expect_equal(max(wide$outcome), 69)

  long <- roll_outcome_count(40 * d6)

  expect_equal(nrow(long), 201)
  expect_equal(long$count[1], 1)
  expect_equal(long$count[201], 1)
  expect_equal(min(long$outcome), 40)
  expect_equal(max(long$outcome), 240)

  mult <- roll_outcome_count(d10 * d20)

  expect_equal(nrow(mult), 100)
  expect_equal(mult$count[1], 1)
  expect_equal(mult$count[50], 2)
  expect_equal(max(mult$count), 5)

  unary <- roll_outcome_count(abs(d6 - d20))

  expect_equal(nrow(unary), 20)
  expect_equal(unary$count[1], 6)
  expect_equal(unary$count[20], 1)
  expect_equal(max(unary$outcome), 19)
})
