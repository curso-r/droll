test_that("proll() works", {
  deven <- d(c(2, 4, 6, 8, 10))

  simple <- proll(3, d6)
  expect_equal(simple, 0.5)

  wide <- proll(17, 3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_equal(wide, 0.007083333, tolerance = 1e-6)

  long <- proll(c(100, 150), 40 * d6)
  expect_equal(long[1], 0.0001026946, tolerance = 1e-6)
  expect_equal(long[2], 0.8339263, tolerance = 1e-6)

  mult <- proll(1:200, d10 * d20)
  expect_equal(mult[1], 0.005)
  expect_equal(mult[100], 1)
  expect_equal(min(mult), 0.005)

  unary <- proll(c(10, 18), abs(d6 - d20))
  expect_equal(unary[1], 0.675)
  expect_equal(unary[2], 0.9916667, tolerance = 1e-6)

  kh <- proll(c(1, 20), max(d20, d20))
  expect_equal(kh[1], 0.0025)
  expect_equal(kh[2], 1)

  kl <- proll(c(1, 20), min(d20, d20))
  expect_equal(kl[1], 0.0975)
  expect_equal(kl[2], 1)

  even <- proll(0, deven %% 2)
  expect_equal(even, 1)

  df <- proll(17, r(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4))
  expect_equal(df, 0.007083333, tolerance = 1e-6)
})

test_that("proll() with !lower.tail works", {
  deven <- d(c(2, 4, 6, 8, 10))

  simple <- proll(3, d6, FALSE)
  expect_equal(simple, 0.5)

  wide <- proll(17, 3 * (2 * d6) + d20 + 1 * d10 - 1 + 4, FALSE)
  expect_equal(wide, 0.9929167, tolerance = 1e-6)

  long <- proll(c(100, 150), 40 * d6, FALSE)
  expect_equal(long[1], 0.9998973, tolerance = 1e-6)
  expect_equal(long[2], 0.1660737, tolerance = 1e-6)

  mult <- proll(1:200, d10 * d20, FALSE)
  expect_equal(mult[1], 0.995)
  expect_equal(mult[100], 0)

  unary <- proll(c(10, 18), abs(d6 - d20), FALSE)
  expect_equal(unary[1], 0.325)
  expect_equal(unary[2], 0.008333333, tolerance = 1e-6)

  kh <- proll(c(1, 20), max(d20, d20), FALSE)
  expect_equal(kh[1], 0.9975)
  expect_equal(kh[2], 0)

  kl <- proll(c(1, 20), min(d20, d20), FALSE)
  expect_equal(kl[1], 0.9025)
  expect_equal(kl[2], 0)

  even <- proll(0, deven %% 2, FALSE)
  expect_equal(even, 0)

  df <- proll(17, r(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4), FALSE)
  expect_equal(df, 0.9929167, tolerance = 1e-6)
})
