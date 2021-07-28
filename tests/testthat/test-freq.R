test_that("roll_outcome_freq() works", {
  d6 <- d(1:6)
  d20 <- d(1:20)
  d10 <- d(1:10)

  wide <- roll_outcome_freq(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)

  expect_equal(nrow(wide), 59)
  expect_equal(wide$freq[30], 0.03888889, tolerance = 1e-6)
  expect_equal(min(wide$outcome), 11)
  expect_equal(max(wide$outcome), 69)

  long <- roll_outcome_freq(40 * d6)

  expect_equal(nrow(long), 201)
  expect_equal(long$freq[1], 7.480833e-32, tolerance = 1e-6)
  expect_equal(long$freq[201], 7.480833e-32, tolerance = 1e-6)
  expect_equal(min(long$outcome), 40)
  expect_equal(max(long$outcome), 240)

  mult <- roll_outcome_freq(d10 * d20)

  expect_equal(nrow(mult), 100)
  expect_equal(mult$freq[1], 0.005)
  expect_equal(mult$freq[50], 0.01)
  expect_equal(max(mult$freq), 0.025)

  unary <- roll_outcome_freq(abs(d6 - d20))

  expect_equal(nrow(unary), 20)
  expect_equal(unary$freq[1], 0.05)
  expect_equal(unary$freq[20], 0.008333333, tolerance = 1e-6)
  expect_equal(max(unary$outcome), 19)

  kh <- roll_outcome_freq(max(d20, d20))

  expect_equal(nrow(kh), 20)
  expect_equal(kh$freq[20], 0.0975)
  expect_equal(kh$freq[1], 0.0025)

  kl <- roll_outcome_freq(min(d20, d20))

  expect_equal(nrow(kl), 20)
  expect_equal(kl$freq[20], 0.0025)
  expect_equal(kl$freq[1], 0.0975)

  deven <- d(c(2, 4, 6, 8, 10))
  even <- roll_outcome_freq(deven %% 2)

  expect_equal(nrow(even), 1)
  expect_equal(even$freq, 1)
  expect_equal(even$outcome, 0)
})

test_that("arbitrary precision works", {
  d6 <- d(1:6)

  long <- roll_outcome_freq(40 * d6, TRUE)

  expect_type(long$freq, "character")
  expect_equal(nrow(long), 201)
  expect_equal(long$freq[1], "1/13367494538843734067838845976576")
  expect_equal(long$freq[201], "1/13367494538843734067838845976576")
  expect_equal(min(long$outcome), 40)
  expect_equal(max(long$outcome), 240)
  expect_equal(
    long$freq[100],
    "15303026976766994740840786585/417734204338866689619963936768"
  )
})
