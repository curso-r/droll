test_that("rroll() works", {
  expect_length(rroll(10, 40 * d6), 10)
  expect_length(rroll(0, 40 * d6), 0)
  expect_error(rroll(-1, 40 * d6), "Invalid arguments")
})
