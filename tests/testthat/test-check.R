test_that("check_*() works", {

  expect_equal(check_pass(max(d20, d20) + 8 + d4, 19), 0.85625)
  expect_equal(check_fail(max(d20, d20) + 8 + d4, 19), 0.14375)

  expect_equal(check_pass(min(d20, d20) + 8 + d4, 19), 0.39375)
  expect_equal(check_fail(min(d20, d20) + 8 + d4, 19), 0.60625)
})