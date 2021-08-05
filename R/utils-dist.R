
#' Calculate the probability mass function of a roll
#'
#' @param x A numeric vector of outcomes.
#' @param df A data frame returned by [r()].
#' @return A numeric vector.
#'
#' @noRd
roll_pdf <- function(x, df) {

  # Calculate f(x)
  yac_n(df$d[df$outcome %in% x])
}

#' Calculate the cumulative distribution function of a roll
#'
#' @param q A numeric vector of outcomes.
#' @param df A data frame returned by [r()].
#' @param lower.tail Whether to calculate `P[X <= x]` or `P[X > x]`.
#' @return A numeric vector.
#'
#' @noRd
roll_cdf <- function(q, df, lower.tail = TRUE) {

  # Calculate F(q)
  out <- yac_n(df$p[df$outcome %in% q])

  # Handle side of tail
  if (!lower.tail) {
    out <- 1 - out
  }

  return(out)
}

#' Calculate the quantile function of a roll
#'
#' @param p A numeric vector of probabilities.
#' @param df A data frame returned by [r()].
#' @param lower.tail Whether to calculate `P[X <= x]` or `P[X > x]`.
#' @return A numeric vector.
#'
#' @noRd
roll_quantile <- function(p, df, lower.tail = TRUE) {

  # Convert to numeric
  df$p <- yac_n(df$p)

  # Handle side of tail
  if (!lower.tail) {
    p <- 1 - p
  }

  # Calculate q(p)
  out <- c()
  for (p_ in p) {
    out <- append(out, df$outcome[min(which(df$p >= p_))])
  }

  return(out)
}

#' Generate random deviates of a roll
#'
#' @param n Number of random deviates to return.
#' @param df A data frame returned by [r()].
#' @param lower.tail Whether to calculate `P[X <= x]` or `P[X > x]`.
#' @return A numeric vector.
#'
#' @noRd
roll_rand <- function(n, roll, env = parent.frame()) {

  # Handle edge cases
  if (n == 0) {
    return(numeric(0))
  } else if (n < 0) {
    stop("Invalid arguments")
  }

  # Evaluate expression n times
  out <- c()
  for (i in 1:n) {
    out <- append(out, eval_dice(roll_dice(roll, env), env))
  }

  return(out)
}
