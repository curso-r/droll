test_that("plotting functions works", {
  d6 <- d(1:6)
  d20 <- d(1:20)
  d10 <- d(1:10)

  d <- droll_plot(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_snapshot(d)

  p <- proll_plot(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_snapshot(p)

  p_ <- proll_plot(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4, lower.tail = FALSE)
  expect_snapshot(p_)

  q <- qroll_plot(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_snapshot(q)

  q_ <- qroll_plot(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4, lower.tail = FALSE)
  expect_snapshot(q_)

  # Skip due to different seed Behavior
  skip_if(R.version$major < 4)
  set.seed(42)

  r <- rroll_plot(1000, 3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_snapshot(r)
})
