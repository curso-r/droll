
# Global regex for matching dice
dice_regex <- "([0-9]*?)[dD]([0-9]+)"

#' Roll dice
#' @param sides Sides
#' @param n N
d <- function(sides, n = 1) {
  sum(sample(1:sides, n, TRUE))
}

# Roll dice
`%d%` <- function(x, y) {
  d(x, y)
}
