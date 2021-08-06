test_that("Dice S4 class works", {
  d6 <- d(6)
  d10 <- d(10)
  d20 <- d(20)

  expect_output(print(d6), "# A die with faces:")
  expect_length(d6, 6)

  expect_lte(1 * d6, 6)
  expect_lte(1 * d10, 10)
  expect_lte(1 * d20, 20)
  expect_lte(d20 * d10, 200)

  expect_gte(1 * d6, 1)
  expect_gte(1 * d10, 1)
  expect_gte(1 * d20, 1)
  expect_gte(d20 * d10, 1)

  expect_equal((d6 * 10) %% 10, 0)
  expect_true(any((sapply(1:10, function(x) 10 * d6) %% 10) != 0))

  expect_type(10 + d6, "double")
  expect_type(d6 + 10, "double")
  expect_type(10 / d6, "double")
  expect_type(d6 / 10, "double")
  expect_type(10 - d6, "double")
  expect_type(d6 - 10, "double")
  expect_type(10^d6, "double")
  expect_type(d6^10, "double")
  expect_type(10 %% d6, "double")
  expect_type(d6 %% 10, "double")
  expect_type(10 %/% d6, "double")
  expect_type(d6 %/% 10, "double")

  expect_type(sqrt(d6), "double")
  expect_type(log2(d6), "double")

  expect_type(round(d6), "double")

  expect_type(max(d6, d6), "double")
  expect_equal(max(d6, 10), 10)
  expect_type(sum(d6, d6), "double")
  expect_type(prod(d6, d6), "double")

  deven <- d(c(2, 4, 6, 8, 10))
  expect_equal(deven %% 2, 0)

  expect_length(c(d6, d10, d20), 36)
  expect_length(c(d6, d10, d20, 1, 2), 38)

  expect_type(as.numeric(d6), "double")
})
