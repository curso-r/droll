test_that("check_prob() works", {
  expect_equal(check_prob(d20, 21), 0)
  expect_equal(check_prob(d20, 0), 0)

  expect_equal(check_prob(d20, 11), 0.5)
  expect_equal(check_prob(d20, 11, FALSE), 0.5)

  expect_equal(check_prob(max(d20, d20) + 8 + d4, 19), 0.85625)
  expect_equal(check_prob(max(d20, d20) + 8 + d4, 19, FALSE), 0.14375)

  expect_equal(check_prob(min(d20, d20) + 8 + d4, 19), 0.39375)
  expect_equal(check_prob(min(d20, d20) + 8 + d4, 19, FALSE), 0.60625)
})

test_that("check_dc() works", {
  expect_equal(check_dc(d20, 0), 20)
  expect_equal(check_dc(d20, 1), 1)

  expect_equal(check_dc(d20, 0.5), 11)
  expect_equal(check_dc(d20, 0.5, FALSE), 11)

  expect_equal(check_dc(max(d20, d20) + 8 + d4, 0.85625), 19)
  expect_equal(check_dc(max(d20, d20) + 8 + d4, 0.14375, FALSE), 18)

  expect_equal(check_dc(min(d20, d20) + 8 + d4, 0.39375), 19)
  expect_equal(check_dc(min(d20, d20) + 8 + d4, 0.60625, FALSE), 19)
})
