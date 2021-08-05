test_that("qroll() works", {
  deven <- d(c(2, 4, 6, 8, 10))

  simple <- qroll(0.5, d6)
  expect_equal(simple, 3)

  wide <- qroll(0.007083333, 3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_equal(wide, 17)

  long <- qroll(c(0.0001026946, 0.8339263), 40 * d6)
  expect_equal(long[1], 100)
  expect_equal(long[2], 150)

  mult <- qroll(c(0.005, 1), d10 * d20)
  expect_equal(mult[1], 1)
  expect_equal(mult[2], 200)

  unary <- qroll(c(0.675, 0.9916666), abs(d6 - d20))
  expect_equal(unary[1], 10)
  expect_equal(unary[2], 18)

  kh <- qroll(c(0.0025, 1), max(d20, d20))
  expect_equal(kh[1], 1)
  expect_equal(kh[2], 20)

  kl <- qroll(c(0.0975, 1), min(d20, d20))
  expect_equal(kl[1], 1)
  expect_equal(kl[2], 20)

  even <- qroll(1, deven %% 2)
  expect_equal(even, 0)

  df <- qroll(0.007083333, r(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4))
  expect_equal(df, 17)
})

test_that("qroll() with !lower.tail works", {
  deven <- d(c(2, 4, 6, 8, 10))

  simple <- qroll(0.5, d6, FALSE)
  expect_equal(simple, 3)

  wide <- qroll(0.9929167, 3 * (2 * d6) + d20 + 1 * d10 - 1 + 4, FALSE)
  expect_equal(wide, 17)

  long <- qroll(c(0.9998973, 0.1660737), 40 * d6, FALSE)
  expect_equal(long[1], 101)
  expect_equal(long[2], 150)

  mult <- qroll(c(0.995, 0), d10 * d20, FALSE)
  expect_equal(mult[1], 2)
  expect_equal(mult[2], 200)

  unary <- qroll(c(0.325, 0.008333333), abs(d6 - d20), FALSE)
  expect_equal(unary[1], 10)
  expect_equal(unary[2], 19)

  kh <- qroll(c(0.9975, 0), max(d20, d20), FALSE)
  expect_equal(kh[1], 1)
  expect_equal(kh[2], 20)

  kl <- qroll(c(0.9025, 0), min(d20, d20), FALSE)
  expect_equal(kl[1], 2)
  expect_equal(kl[2], 20)

  even <- qroll(0, deven %% 2, FALSE)
  expect_equal(even, 0)

  df <- qroll(0.9929166, r(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4), FALSE)
  expect_equal(df, 18)
})
