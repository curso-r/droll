test_that("droll() works", {
  deven <- d(c(2, 4, 6, 8, 10))

  simple <- droll(1, d6)
  expect_equal(simple, 0.1666667, tolerance = 1e-6)

  wide <- droll(40, 3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_equal(wide, 0.03888889, tolerance = 1e-6)

  long <- droll(c(40, 240), 40 * d6)
  expect_equal(long[1], 7.480833e-32, tolerance = 1e-6)
  expect_equal(long[2], 7.480833e-32, tolerance = 1e-6)

  mult <- droll(1:63, d10 * d20)
  expect_equal(mult[1], 0.005)
  expect_equal(mult[50], 0.01)
  expect_equal(max(mult), 0.025)

  unary <- droll(c(0, 19), abs(d6 - d20))
  expect_equal(unary[1], 0.05)
  expect_equal(unary[2], 0.008333333, tolerance = 1e-6)

  kh <- droll(c(1, 20), max(d20, d20))
  expect_equal(kh[1], 0.0025)
  expect_equal(kh[2], 0.0975)

  kl <- droll(c(1, 20), min(d20, d20))
  expect_equal(kl[1], 0.0975)
  expect_equal(kl[2], 0.0025)

  even <- droll(0, deven %% 2)
  expect_equal(even, 1)

  df <- droll(40, r(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4))
  expect_equal(df, 0.03888889, tolerance = 1e-6)
})
