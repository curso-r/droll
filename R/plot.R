
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
#' @seealso [graphics::barplot()], [ggplot2::qplot()], [d()], [roll]
#'
#' @param n Number of random deviates to return.
#' @param roll A roll expression (e.g., `2 * d6 + 5`) or a data frame returned
#' by [r()].
#' @param lower.tail Whether to calculate `P[X <= x]` or `P[X > x]`.
#' @param ... Other arguments passed on to plotting functions
#' ([graphics::barplot()] or [ggplot2::qplot()] if available).
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
#' @name roll-plot
NULL

#' @rdname roll-plot
#' @export
droll_plot <- function(roll, ...) {

  # Get full distribution and f(x)
  df <- roll_outcome_count(substitute(roll), parent.frame())
  y <- roll_pdf(df$outcome, df)

  if (is_ggplot2_installed()) {
    out <- ggplot2::qplot(
      df$outcome, y, ...,
      geom = "col",
      xlab = "Outcome", ylab = "P[X = x]"
    )
  } else {
    out <- graphics::barplot(
      y,
      names.arg = df$outcome,
      xlab = "Outcome", ylab = "P[X = x]", ...
    )
  }

  return(out)
}

#' @rdname roll-plot
#' @export
proll_plot <- function(roll, lower.tail = TRUE, ...) {

  # Get full distribution and F(q)
  df <- roll_outcome_count(substitute(roll), parent.frame())
  y <- roll_cdf(df$outcome, df, lower.tail)

  if (is_ggplot2_installed()) {
    out <- ggplot2::qplot(
      df$outcome, y, ...,
      geom = "col",
      xlab = "Outcome", ylab = if (lower.tail) "P[X <= x]" else "P[X > x]"
    )
  } else {
    out <- graphics::barplot(
      y,
      names.arg = df$outcome,
      xlab = "Outcome", ylab = if (lower.tail) "P[X <= x]" else "P[X > x]", ...
    )
  }

  return(out)
}

#' @rdname roll-plot
#' @export
qroll_plot <- function(roll, lower.tail = TRUE, ...) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get q(p)
  p <- seq(0, 1, length.out = nrow(df))
  y <- roll_quantile(p, df, lower.tail)

  if (is_ggplot2_installed()) {
    out <- ggplot2::qplot(
      p, y, ...,
      geom = "col",
      xlab = if (lower.tail) "P[X <= x]" else "P[X > x]", ylab = "Outcome"
    )
  } else {
    out <- graphics::barplot(
      y,
      names.arg = round(p, 2),
      xlab = if (lower.tail) "P[X <= x]" else "P[X > x]", ylab = "Outcome", ...
    )
  }

  return(out)
}

#' @rdname roll-plot
#' @export
rroll_plot <- function(n, roll, ...) {

  # Get n deviates
  devs <- roll_rand(n, substitute(roll), parent.frame())

  # Convert deviates to table
  devs <- table(devs)

  if (is_ggplot2_installed()) {
    out <- ggplot2::qplot(
      as.numeric(names(devs)), as.numeric(devs), ...,
      geom = "col",
      xlab = "Outcome", ylab = "Count"
    )
  } else {
    out <- graphics::barplot(
      as.numeric(devs),
      names.arg = as.numeric(names(devs)),
      xlab = "Outcome", ylab = "Count", ...
    )
  }

  return(out)
}

#' Test if ggplot2 is installed
#'
#' Auxiliary function that tests if ggplot2 is installed. This is necessary for
#' [mockery::stub()] to have something to override.
#'
#' @return A boolean.
#'
#' @noRd
is_ggplot2_installed <- function() {
  requireNamespace("ggplot2", quietly = TRUE)
}
