test_that("plotting functions work", {

  d <- droll_plot(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  ggplot2::ggsave("test-plot/d.png", d, width = 7, height = 7)
  expect_snapshot_file("test-plot/d.png")

  p <- proll_plot(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  ggplot2::ggsave("test-plot/p.png", p, width = 7, height = 7)
  expect_snapshot_file("test-plot/p.png")

  p_ <- proll_plot(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4, lower.tail = FALSE)
  ggplot2::ggsave("test-plot/p_.png", p_, width = 7, height = 7)
  expect_snapshot_file("test-plot/p_.png")

  q <- qroll_plot(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  ggplot2::ggsave("test-plot/q.png", q, width = 7, height = 7)
  expect_snapshot_file("test-plot/q.png")

  q_ <- qroll_plot(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4, lower.tail = FALSE)
  ggplot2::ggsave("test-plot/q_.png", q_, width = 7, height = 7)
  expect_snapshot_file("test-plot/q_.png")

  # Skip due to different seed Behavior
  skip_if(R.version$major < 4)
  set.seed(42)

  r <- rroll_plot(1000, 3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  ggplot2::ggsave("test-plot/r.png", r, width = 7, height = 7)
  expect_snapshot_file("test-plot/r.png")
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

  r <- rroll_plot(1000, 3 * (2 * d6) + d20 + 1 * d10 - 1 + 4)
  expect_snapshot(r)
})
