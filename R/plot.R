
#' Plot the roll distribution
#'
#' @description
#' Plot density, distribution function, quantile function, and random generation
#' for the discrete distribution described by a roll expression. See below for
#' more details.
#'
#' @details
#' Given a roll expression (i.e., an arithmetic expression involving dice),
#' [r()] calculates the complete distribution of the outcomes. This is possible
#' because the distribution is discrete and has a finite number of outcomes.
#'
#' From this distribution, [droll_plot()] plots the density, [proll_plot()]
#' plots the distribution function, [qroll_plot()] plots the quantile function,
#' and [rroll_plot()] plots random deviates.
#'
#' For more information, see the generating functions: [roll].
#'
#' @seealso [graphics::barplot()], [graphics::hist()], [d()], [roll]
#'
#' @param n Number of random deviates to return.
#' @param roll A roll expression (e.g., `2 * d6 + 5`) or a data frame returned
#' by [r()].
#' @param ... Other arguments passed on to [graphics::barplot()] or
#' [graphics::hist()] ([rroll_plot()] only).
#' @param lower.tail Whether to calculate `P[X <= x]` or `P[X > x]`.
#' @return For [droll_plot()], [proll_plot()], and [qroll_plot()] a bar plot.
#' For [rroll_plot()] a histogram.
#'
#' @examples
#' set.seed(42)
#'
#' # Density of 2d6 + 5
#' droll_plot(2 * d6 + 5)
#'
#' # Distribution function of 2d6 + 5
#' proll_plot(2 * d6 + 5)
#'
#' # Quantile function of 2d6 + 5
#' qroll_plot(2 * d6 + 5)
#'
#' # Roll 2d6 + 5
#' rroll_plot(1000, 2 * d6 + 5)
#'
#' @name roll-plot
NULL

#' @rdname roll-plot
#' @export
droll_plot <- function(roll, ...) {
  df <- roll_outcome_count(substitute(roll), parent.frame())

  graphics::barplot(
    yac_n(df$d), names.arg = df$outcome,
    xlab = "Outcome", ylab = "P[X = x]", ...
  )
}

#' @rdname roll-plot
#' @export
proll_plot <- function(roll, ..., lower.tail = TRUE) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  tails <- c()
  for (n in df$outcome) {

    # Handle side of tail
    if (lower.tail) {
      tail <- df$d[df$outcome <= n]
    } else {
      tail <- df$d[df$outcome > n]
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

#' @rdname roll-plot
#' @export
qroll_plot <- function(roll, ..., lower.tail = TRUE) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Calculate the cumulative sum of the probabilities
  freq <- Reduce(
    function(x, y) yac("Add", paste0(x, ",", y)),
    df$d, accumulate = TRUE
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

#' @rdname roll-plot
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
