test_that("plotting functions work", {
  gd <- droll_plot(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  vdiffr::expect_doppelganger("gd", gd)

  gp_ <- proll_plot(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4, lower.tail = FALSE)
  vdiffr::expect_doppelganger("gp_", gp_)

  gq_ <- qroll_plot(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4, lower.tail = FALSE)
  vdiffr::expect_doppelganger("gq_", gq_)

  # Skip due to different seed Behavior
  skip_if(R.version$major < 4)
  set.seed(42)

  gr <- rroll_plot(1000, 3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  vdiffr::expect_doppelganger("gr", gr)
})

test_that("plotting functions work without ggplot2", {

  # Mock absence of ggplot2
  mockery::stub(droll_plot, "is_ggplot2_installed", function() FALSE)
  mockery::stub(proll_plot, "is_ggplot2_installed", function() FALSE)
  mockery::stub(qroll_plot, "is_ggplot2_installed", function() FALSE)
  mockery::stub(rroll_plot, "is_ggplot2_installed", function() FALSE)

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

  r <- rroll_plot(10000, 3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_snapshot(r)
})
