
#' Count the ways each outcome of a roll can be obtained
#'
#' Given a roll expression (i.e. an arithmetic expression involving dice),
#' compute the absolute frequency of each possible outcome. In other words,
#' calculate how many ways every outcome of the roll can be obtained.
#'
#' @param roll A roll expression (e.g. `2 * d6 + 5`).
#' @param precise Whether to return values with arbitrary precision.
#' @return A data frame with two columns: `outcome`, `count`, and `freq`.
#'
#' @examples
#' # Possible outcomes of 2d6 + 5
#' d6 <- d(1:6)
#' roll(2 * d6 + 5)
#' @export
roll <- function(roll, precise = FALSE) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Convert values to numeric if requested
  if (!precise) {
    df$freq <- yac_n(df$freq)
    df$count <- yac_n(df$count)
  }

  return(df)
}

#' Get relative frequency of an outcome of a roll
#'
#' Given a roll expression (i.e. an arithmetic expression involving dice),
#' compute the relative frequency of a specific outcome. In other words,
#' calculate the probability of obtaining this outcome.
#'
#' @param x A vector of outcomes.
#' @param roll A roll expression (e.g. `2 * d6 + 5`).
#' @return A numeric vector.
#'
#' @examples
#' # Possible outcomes of 2d6 + 5
#' d6 <- d(1:6)
#' droll(7:9, 2 * d6 + 5)
#' @export
droll <- function(x, roll) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Filter correct frequencies and convert to numeric
  yac_n(df$freq[df$outcome %in% x])
}

#' Get distribution of outcomes of a roll
#'
#' Given a roll expression (i.e. an arithmetic expression involving dice),
#' compute the distribution the outcomes, returning `P[X <= x]` where `x` is an
#' outcome of the roll.
#'
#' @param q A vector of outcomes.
#' @param roll A roll expression (e.g. `2 * d6 + 5`).
#' @param lower.tail Whether to return `P[X ≤ x]` or `P[X > x]`.
#' @return A numeric vector.
#'
#' @examples
#' # Possible outcomes of 2d6 + 5
#' d6 <- d(1:6)
#' proll(7:9, 2 * d6 + 5)
#' @export
proll <- function(q, roll, lower.tail = TRUE) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  tails <- c()
  for (n in q) {

    # Handle side of tail
    if (lower.tail) {
      tail <- df$freq[df$outcome <= n]
    } else {
      tail <- df$freq[df$outcome > n]
    }

    # Add and convert to numeric
    tails <- append(tails, yac_n(yac("Add", paste0(tail, collapse = ","))))
  }

  return(tails)
}

#' Get inverse distribution of outcomes of a roll
#'
#' Given a roll expression (i.e. an arithmetic expression involving dice),
#' compute the distribution the outcomes, returning `x | P[X ≤ x] = p` where
#' `p` is a probability.
#'
#' @param p A vector of probabilities.
#' @param roll A roll expression (e.g. `2 * d6 + 5`).
#' @param lower.tail Whether to return `x | P[X ≤ x] = p` or `x | P[X > x] = p`.
#' @return A numeric vector.
#'
#' @examples
#' # Possible outcomes of 2d6 + 5
#' d6 <- d(1:6)
#' qroll(7:9, 2 * d6 + 5)
#' @export
qroll <- function(p, roll, lower.tail = TRUE) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Calculate the cumulative sum of the probabilities
  freq <- Reduce(
    function(x, y) yac("Add", paste0(x, ",", y)),
    if (lower.tail) df$freq else rev(df$freq), accumulate = TRUE
  )

  # Convert to numeric
  freq <- yac_n(freq)

  # Get outcomes that correspond to p
  tails <- c()
  for (f in p) {

    # Handle side of tail
    if (lower.tail) {
      tail <- df$outcome[which.min(freq <= f) - 1]
    } else {
      tail <- rev(df$outcome)[which.min(freq < f) - 1]
    }

    tails <- append(tails, tail)
  }

  return(tails)
}

#' Roll some dice
#'
#' Given a roll expression (i.e. an arithmetic expression involving dice),
#' compute an outcome by sampling one face from each die and evaluating the
#' expression. If `verbose = TRUE`, also print the expression before evaluation.
#'
#' @param n Number of observations.
#' @param roll A roll expression (e.g. `2 * d6 + 5`).
#' @param verbose Whether to print the expression after the dice rolls.
#' @return A numeric vector.
#'
#' @examples
#' # Roll 2d6 + 6
#' d6 <- d(1:6)
#' rroll(3, 2 * d6 + 5)
#' @export
rroll <- function(n, roll, verbose = TRUE) {

  # Handle edge cases
  if (n == 0) {
    return(numeric(0))
  } else if (n < 0) {
    stop("Invalid arguments")
  }

  # Evaluate expression n times
  out <- c()
  for (i in 1:n) {

    # Roll dice making each result explicit
    expr <- roll_dice(substitute(roll), parent.frame())
    out <- append(out, eval(expr, parent.frame()))
  }

  # Print expression for user befor evaluating
  if (verbose && n == 1) {
    cat(
      "\033[38;5;246m# Outcome:",
      gsub(" +", " ", paste0(deparse(expr), collapse = "")),
      "\n\033[39m"
    )
  }

  return(out)
}
