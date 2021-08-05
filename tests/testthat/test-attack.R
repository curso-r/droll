test_that("attack_*() works", {

  expect_equal(attack_hit(max(d20, d20) + 8 + d4, 19), 0.85625)
  expect_equal(attack_miss(max(d20, d20) + 8 + d4, 19), 0.14375)

  expect_equal(attack_hit(min(d20, d20) + 8 + d4, 19), 0.39375)
  expect_equal(attack_miss(min(d20, d20) + 8 + d4, 19), 0.60625)

  expect_equal(attack_critical_hit(max(d20, d20)), 0.0975)
  expect_equal(attack_critical_miss(max(d20, d20)), 0.0025)

  expect_equal(attack_critical_hit(min(d20, d20)), 0.0025)
  expect_equal(attack_critical_miss(min(d20, d20)), 0.0975)
})

test_that("check_*() works", {

  expect_equal(check_pass(max(d20, d20) + 8 + d4, 19), 0.85625)
  expect_equal(check_fail(max(d20, d20) + 8 + d4, 19), 0.14375)

  expect_equal(check_pass(min(d20, d20) + 8 + d4, 19), 0.39375)
  expect_equal(check_fail(min(d20, d20) + 8 + d4, 19), 0.60625)

  expect_equal(check_critical_pass(max(d20, d20)), 0.0975)
  expect_equal(check_critical_fail(max(d20, d20)), 0.0025)

  expect_equal(check_critical_pass(min(d20, d20)), 0.0025)
  expect_equal(check_critical_fail(min(d20, d20)), 0.0975)
})