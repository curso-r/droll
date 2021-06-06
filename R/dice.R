
drg <- function(sides, n = 1) {
  (1*n):(sides*n)
}

d <- function(sides, n = 1) {
  sum(sample(1:sides, n, TRUE))
}

`%d%` <- function(x, y) {
  d(x, y)
}
