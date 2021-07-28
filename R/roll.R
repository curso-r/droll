
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

  # Calculate absolute density distribution
  df <- roll_outcome_count(substitute(roll), TRUE, parent.frame())
  t <- yac("Add", paste0(df$count, collapse = ","))

  # Calculate relative frequency with arbitrary precision
  df$freq <- sapply(df$count, function(n) yac("", paste0(n, "/", t)))
  names(df$freq) <- NULL

  # Convert values to numeric if requested
  if (!precise) {
    df$freq <- sapply(df$freq, function(f) yac("N", f))
    df$freq <- as.numeric(df$freq)
    df$count <- sapply(df$count, function(n) yac("N", n))
    df$count <- as.numeric(df$count)
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
  df <- roll_outcome_count(substitute(roll), TRUE, parent.frame())
  t <- yac("Add", paste0(df$count, collapse = ","))

  # Calculate relative frequency with arbitrary precision
  df$freq <- sapply(df$count, function(n) yac("", paste0(n, "/", t)))
  names(df$freq) <- NULL

  # Filter correct frequencies and convert to numeric
  out <- df$freq[df$outcome %in% x]
  as.numeric(sapply(out, function(f) yac("N", f)))
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
