test_that("roll() for outcomes works", {
  d6 <- d(1:6)
  d20 <- d(1:20)
  d10 <- d(1:10)
  deven <- d(c(2, 4, 6, 8, 10))

  simple <- roll(d6)
  expect_equal(nrow(simple), 6)
  expect_equal(min(simple$outcome), 1)
  expect_equal(max(simple$outcome), 6)

  wide <- roll(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_equal(nrow(wide), 59)
  expect_equal(min(wide$outcome), 11)
  expect_equal(max(wide$outcome), 69)

  long <- roll(40 * d6)
  expect_equal(nrow(long), 201)
  expect_equal(min(long$outcome), 40)
  expect_equal(max(long$outcome), 240)

  mult <- roll(d10 * d20)
  expect_equal(nrow(mult), 100)

  unary <- roll(abs(d6 - d20))
  expect_equal(nrow(unary), 20)
  expect_equal(max(unary$outcome), 19)

  kh <- roll(max(d20, d20))
  expect_equal(nrow(kh), 20)

  kl <- roll(min(d20, d20))
  expect_equal(nrow(kl), 20)

  even <- roll(deven %% 2)
  expect_equal(nrow(even), 1)
  expect_equal(even$outcome, 0)
})

test_that("roll() for counts works", {
  d6 <- d(1:6)
  d20 <- d(1:20)
  d10 <- d(1:10)
  deven <- d(c(2, 4, 6, 8, 10))

  simple <- roll(d6)
  expect_true(all(simple$count == 1))

  wide <- roll(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_equal(wide$count[30], 280)

  long <- roll(40 * d6)
  expect_equal(long$count[1], 1)
  expect_equal(long$count[201], 1)

  mult <- roll(d10 * d20)
  expect_equal(mult$count[1], 1)
  expect_equal(mult$count[50], 2)
  expect_equal(max(mult$count), 5)

  unary <- roll(abs(d6 - d20))
  expect_equal(unary$count[1], 6)
  expect_equal(unary$count[20], 1)

  kh <- roll(max(d20, d20))
  expect_equal(kh$count[20], 39)
  expect_equal(kh$count[1], 1)

  kl <- roll(min(d20, d20))
  expect_equal(kl$count[20], 1)
  expect_equal(kl$count[1], 39)

  even <- roll(deven %% 2)
  expect_equal(even$count, 5)
})

test_that("roll() for frequencies works", {
  d6 <- d(1:6)
  d20 <- d(1:20)
  d10 <- d(1:10)
  deven <- d(c(2, 4, 6, 8, 10))

  simple <- roll(d6)
  expect_equal(simple$freq[1], 0.1666667, tolerance = 1e-6)

  wide <- roll(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_equal(wide$freq[30], 0.03888889, tolerance = 1e-6)

  long <- roll(40 * d6)
  expect_equal(long$freq[1], 7.480833e-32, tolerance = 1e-6)
  expect_equal(long$freq[201], 7.480833e-32, tolerance = 1e-6)

  mult <- roll(d10 * d20)
  expect_equal(mult$freq[1], 0.005)
  expect_equal(mult$freq[50], 0.01)
  expect_equal(max(mult$freq), 0.025)

  unary <- roll(abs(d6 - d20))
  expect_equal(unary$freq[1], 0.05)
  expect_equal(unary$freq[20], 0.008333333, tolerance = 1e-6)

  kh <- roll(max(d20, d20))
  expect_equal(kh$freq[20], 0.0975)
  expect_equal(kh$freq[1], 0.0025)

  kl <- roll(min(d20, d20))
  expect_equal(kl$freq[20], 0.0025)
  expect_equal(kl$freq[1], 0.0975)

  even <- roll(deven %% 2)
  expect_equal(even$freq, 1)
})

test_that("arbitrary precision works", {
  d6 <- d(1:6)

  long <- roll(40 * d6, TRUE)

  expect_type(long$freq, "character")
  expect_equal(nrow(long), 201)
  expect_equal(long$count[1], "1")
  expect_equal(long$count[201], "1")
  expect_equal(long$freq[1], "1/13367494538843734067838845976576")
  expect_equal(long$freq[201], "1/13367494538843734067838845976576")
  expect_equal(min(long$outcome), 40)
  expect_equal(max(long$outcome), 240)
  expect_equal(long$count[100], "489696863256543831706905170720")
  expect_equal(
    long$freq[100],
    "15303026976766994740840786585/417734204338866689619963936768"
  )
})

test_that("exotic cases work", {
  d10 <- d(1:10)
  d20 <- d(1:20)

  # Level 6 barbarian raging and attacking recklessly
  barbarian <- roll({
    if (max(d20, d20) + 8 > 15) {
      return(d10 + 7)
    } else {
      return(0)
    }
  })

  expect_equal(nrow(barbarian), 11)
  expect_equal(barbarian$freq[1], 0.1225)
  expect_true(all(barbarian$freq[-1] == 0.08775))
})
