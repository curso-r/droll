
#' Plot the ways each outcome of a roll can be obtained
#'
#' Given a roll expression (i.e. an arithmetic expression involving dice),
#' compute the absolute frequency of each possible outcome and plot them as a
#' bar chart.
#'
#' @param roll A roll expression (e.g. `2 * d6 + 5`).
#' @param ... Other arguments passed on to [graphics::barplot()]
#' @return A bar plot.
#'
#' @examples
#' # Possible outcomes of 2d6 + 5
#' d6 <- d(1:6)
#' droll_plot(2 * d6 + 5)
#' @export
droll_plot <- function(roll, ...) {
  df <- roll_outcome_count(substitute(roll), parent.frame())

  graphics::barplot(
    yac_n(df$freq), names.arg = df$outcome,
    xlab = "Outcome", ylab = "P[X = x]", ...
  )
}

#' Plot probability distribution
#'
#' @param roll A roll expression (e.g. `2 * d6 + 5`).
#' @param ... Other arguments passed on to [graphics::barplot()]
#' @param lower.tail Whether to return `P[X <= x]` or `P[X > x]`.
#' @return A bar plot.
#'
#' @examples
#' # Possible outcomes of 2d6 + 5
#' d6 <- d(1:6)
#' proll_plot(2 * d6 + 5)
#' @export
proll_plot <- function(roll, ..., lower.tail = TRUE) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  tails <- c()
  for (n in df$outcome) {

    # Handle side of tail
    if (lower.tail) {
      tail <- df$freq[df$outcome <= n]
    } else {
      tail <- df$freq[df$outcome > n]
    }

    # Handle empty results
    if (length(tail) == 0) {
      tail <- "0"
    }

    # Convert to numeric
    tails <- append(tails, yac_n(yac("Add", paste0(tail, collapse = ","))))
  }

  graphics::barplot(
    tails, names.arg = df$outcome,
    xlab = "Outcome", ylab = if (lower.tail) "P[X <= x]" else "P[X > x]", ...
  )
}

#' Plot quantile distribution
#'
#' @param roll A roll expression (e.g. `2 * d6 + 5`).
#' @param ... Other arguments passed on to [graphics::barplot()]
#' @param lower.tail Whether to return `x | P[X <= x] >= p` or
#' `x | P[X > x] >= p`.
#' @return A bar plot.
#'
#' @examples
#' # Possible outcomes of 2d6 + 5
#' d6 <- d(1:6)
#' qroll_plot(2 * d6 + 5)
#' @export
qroll_plot <- function(roll, ..., lower.tail = TRUE) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Calculate the cumulative sum of the probabilities
  freq <- Reduce(
    function(x, y) yac("Add", paste0(x, ",", y)),
    df$freq, accumulate = TRUE
  )

  # Handle side of tail
  if (!lower.tail) {
    freq <- sapply(freq, function(x) paste0("1-", x))
  }

  # Convert to numeric
  freq <- yac_n(freq)

  # Create vector of probabilities
  if (lower.tail) {
    p <- seq(0, 1, length.out = length(freq))
  } else {
    p <- seq(0, 0.99, length.out = length(freq))
  }

  # Get outcomes that correspond to p
  tails <- c()
  for (f in p) {

    # Handle side of tail
    if (lower.tail) {
      tail <- df$outcome[min(which(freq >= f))]
    } else {
      tail <- df$outcome[max(which(freq >= f))]
    }

    tails <- append(tails, tail)
  }

  graphics::barplot(
    tails, names.arg = round(p, 2),
    xlab = if (lower.tail) "P[X <= x]" else "P[X > x]", ylab = "Outcome", ...
  )
}

#' Plot random samples of a roll
#'
#' @param n Number of samples.
#' @param roll A roll expression (e.g. `2 * d6 + 5`).
#' @param ... Other arguments passed on to [graphics::hist()]
#' @return A histogram.
#'
#' @examples
#' # Possible outcomes of 2d6 + 5
#' d6 <- d(1:6)
#' rroll_plot(1000, 2 * d6 + 5)
#' @export
rroll_plot <- function(n, roll, ...) {

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

  graphics::hist(
    out, breaks = min(out):max(out),
    xlab = "Outcome", ylab = "Count", ...
  )
}
