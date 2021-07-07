test_that("roll_outcome_count() works", {
  d6 <- d(1:6)
  d20 <- d(1:20)
  d10 <- d(1:10)

  wide <- roll_outcome_count(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)

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

  kh <- roll_outcome_count(max(d20, d20))

  expect_equal(nrow(kh), 20)
  expect_equal(kh$count[20], 39)
  expect_equal(kh$count[1], 1)

  kl <- roll_outcome_count(min(d20, d20))

  expect_equal(nrow(kl), 20)
  expect_equal(kl$count[20], 1)
  expect_equal(kl$count[1], 39)

  dEven <- d(c(2, 4, 6, 8, 10))
  even <- roll_outcome_count(dEven %% 2)

  expect_equal(nrow(even), 1)
  expect_equal(even$count, 5)
  expect_equal(even$outcome, 0)
})
