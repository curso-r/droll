test_that("r() for outcomes works", {
  deven <- d(c(2, 4, 6, 8, 10))

  simple <- r(d6)
  expect_equal(nrow(simple), 6)
  expect_equal(min(simple$outcome), 1)
  expect_equal(max(simple$outcome), 6)

  wide <- r(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_equal(nrow(wide), 59)
  expect_equal(min(wide$outcome), 11)
  expect_equal(max(wide$outcome), 69)

  long <- r(40 * d6)
  expect_equal(nrow(long), 201)
  expect_equal(min(long$outcome), 40)
  expect_equal(max(long$outcome), 240)

  mult <- r(d10 * d20)
  expect_equal(nrow(mult), 100)

  unary <- r(abs(d6 - d20))
  expect_equal(nrow(unary), 20)
  expect_equal(max(unary$outcome), 19)

  kh <- r(max(d20, d20))
  expect_equal(nrow(kh), 20)

  kl <- r(min(d20, d20))
  expect_equal(nrow(kl), 20)

  even <- r(deven %% 2)
  expect_equal(nrow(even), 1)
  expect_equal(even$outcome, 0)
})

test_that("r() for counts works", {
  deven <- d(c(2, 4, 6, 8, 10))

  simple <- r(d6)
  expect_true(all(simple$n == 1))

  wide <- r(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_equal(wide$n[30], 280)

  long <- r(40 * d6)
  expect_equal(long$n[1], 1)
  expect_equal(long$n[201], 1)

  mult <- r(d10 * d20)
  expect_equal(mult$n[1], 1)
  expect_equal(mult$n[50], 2)
  expect_equal(max(mult$n), 5)

  unary <- r(abs(d6 - d20))
  expect_equal(unary$n[1], 6)
  expect_equal(unary$n[20], 1)

  kh <- r(max(d20, d20))
  expect_equal(kh$n[20], 39)
  expect_equal(kh$n[1], 1)

  kl <- r(min(d20, d20))
  expect_equal(kl$n[20], 1)
  expect_equal(kl$n[1], 39)

  even <- r(deven %% 2)
  expect_equal(even$n, 5)
})

test_that("r() for frequencies works", {
  deven <- d(c(2, 4, 6, 8, 10))

  simple <- r(d6)
  expect_equal(simple$d[1], 0.1666667, tolerance = 1e-6)

  wide <- r(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_equal(wide$d[30], 0.03888889, tolerance = 1e-6)

  long <- r(40 * d6)
  expect_equal(long$d[1], 7.480833e-32, tolerance = 1e-6)
  expect_equal(long$d[201], 7.480833e-32, tolerance = 1e-6)

  mult <- r(d10 * d20)
  expect_equal(mult$d[1], 0.005)
  expect_equal(mult$d[50], 0.01)
  expect_equal(max(mult$d), 0.025)

  unary <- r(abs(d6 - d20))
  expect_equal(unary$d[1], 0.05)
  expect_equal(unary$d[20], 0.008333333, tolerance = 1e-6)

  kh <- r(max(d20, d20))
  expect_equal(kh$d[20], 0.0975)
  expect_equal(kh$d[1], 0.0025)

  kl <- r(min(d20, d20))
  expect_equal(kl$d[20], 0.0025)
  expect_equal(kl$d[1], 0.0975)

  even <- r(deven %% 2)
  expect_equal(even$d, 1)
})

test_that("arbitrary precision works", {
  long <- r(40 * d6, TRUE)

  expect_type(long$d, "character")
  expect_equal(nrow(long), 201)
  expect_equal(long$n[1], "1")
  expect_equal(long$n[201], "1")
  expect_equal(long$d[1], "1/13367494538843734067838845976576")
  expect_equal(long$d[201], "1/13367494538843734067838845976576")
  expect_equal(min(long$outcome), 40)
  expect_equal(max(long$outcome), 240)
  expect_equal(long$n[100], "489696863256543831706905170720")
  expect_equal(
    long$d[100],
    "15303026976766994740840786585/417734204338866689619963936768"
  )
})

test_that("exotic cases work", {

  # Level 6 barbarian raging and attacking recklessly
  barbarian <- r({
    if (max(d20, d20) + 8 > 15) {
      return(d10 + 7)
    } else {
      return(0)
    }
  })

  expect_equal(nrow(barbarian), 11)
  expect_equal(barbarian$d[1], 0.1225)
  expect_true(all(barbarian$d[-1] == 0.08775))

  # Undeclared built-in dice
  builtin <- r(d4 + d6 + d12)

  expect_equal(nrow(builtin), 20)
  expect_equal(builtin$d[1], 0.003472222, tolerance = 1e-6)
  expect_equal(builtin$n[10], 24)
})
