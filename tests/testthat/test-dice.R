test_that("dice works", {

  d6 <- Dice(1:6)
  d20 <- Dice(1:20)
  d10 <- Dice(1:10)

  expect_lte(d6, 6)
  expect_lte(d10, 10)
  expect_lte(d20, 20)
  expect_lte(d20 * d10, 200)

  expect_gte(d6, 1)
  expect_gte(d10, 1)
  expect_gte(d20, 1)
  expect_gte(d20 * d10, 1)

  expect_equal((d6 * 10) %% 10, 0)
  expect_true(any((sapply(1:10, function(x) 10 * d6) %% 10) != 0))

  expect_type(10 + d6, "double")
  expect_type(d6 + 10, "double")
  expect_type(10 / d6, "double")
  expect_type(d6 / 10, "double")
  expect_type(10 - d6, "double")
  expect_type(d6 - 10, "double")
  expect_type(10 ^ d6, "double")
  expect_type(d6 ^ 10, "double")
  expect_type(10 %% d6, "double")
  expect_type(d6 %% 10, "double")
  expect_type(10 %/% d6, "double")
  expect_type(d6 %/% 10, "double")

})
