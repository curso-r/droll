test_that("plotting functions works", {

  d6  <- d(1:6)
  d20 <- d(1:20)
  d10 <- d(1:10)

  wide <- plot_outcome_count(3 * (2 * d6) + d20 + 1*d10 - 1 + 4)

  expect_snapshot(wide)

})
