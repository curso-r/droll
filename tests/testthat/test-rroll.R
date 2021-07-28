test_that("rroll() works", {
  d6 <- d(1:6)
  d20 <- d(1:20)
  d10 <- d(1:10)

  expect_output(
    rroll(3 * (2 * d6) + d20 - 1 + 4),
    "# Outcome: 3 \\* \\(sum\\([0-9, ]+\\)\\) \\+ [0-9]+ \\- 1 \\+ 4"
  )

  expect_output(
    rroll(40 * d6),
    "# Outcome: sum\\([0-9, ]+\\)"
  )

  expect_output(
    rroll(d10 * d20),
    "# Outcome: [0-9]+ \\* [0-9]+"
  )

  expect_output(
    rroll(abs(d6 - d20)),
    "# Outcome: abs\\([0-9] \\- [0-9]+\\)"
  )

  expect_output(
    rroll(max(d20, d20)),
    "# Outcome: max\\([0-9]+, [0-9]+\\)"
  )

  expect_output(
    rroll(min(d20, d20)),
    "# Outcome: min\\([0-9]+, [0-9]+\\)"
  )

  deven <- d(c(2, 4, 6, 8, 10))
  expect_output(
    rroll(deven %% 2),
    "# Outcome: [0-9]+\\%\\%[0-9]"
  )

  expect_output(
    rroll(3 * (2 * d6) + d20 + 1 * d10 - 1 + 4, verbose = FALSE),
    NA
  )
})
