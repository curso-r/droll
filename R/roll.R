
#' Get full distribution of a roll
#'
#' @details
#' This function powers both the [roll] and [roll-plot] family of functions.
#' Given a roll expression (i.e., an arithmetic expression involving dice), it
#' calculates the complete distribution of the outcomes. This is possible
#' because the distribution is discrete and has a finite number of outcomes.
#'
#' [r()] returns a data frame with most information necessary to work with the
#' roll distribution: a column containing the possible outcomes of the roll
#' expression, a column counting how many different ways each outbome can be
#' obtained, a colum with the associated densities, and a column with the
#' associated probabilities.
#'
#' @section Roll Expressions:
#' A roll expression is a piece of R code that describes a dice roll with or
#' without modifiers, e.g., `2 * d6 + 5`.
#'
#' Standard [dice notation](https://en.wikipedia.org/wiki/Dice_notation) should
#' mostly work out of the box, with the notable exception of `NdX`, i.e., "roll
#' `N` dice with `X` faces and add the results". In this case, the user must
#' write `N * dX`; this also means that, when translating "roll a die with `X`
#' faces and multiply the result by `N`" to a roll expression, the user must
#' then write `dX * N`. All other expressions involving dice can usually be
#' pasted straight into these functions.
#'
#' For more details on what operations are supported, see the [Dice] S4 class.
#'
#' @section Built-in Dice:
#' It is possible to define any die with [d()], but some are already built-in.
#' Because of R's restrictions on what kind of object can be exported, they are
#' not readly available for the user, but can be used inside a roll expression
#' nontheless. These are the standard D&D dice: `d4`, `d6`, `d8`, `d10`, `d12`,
#' `d20`, and `d100`.
#'
#' @section Arbitrary Precision:
#' Most dice programs that can calculate probabilities are forced to round
#' their results due to the fact that these quantities might become
#' exceptionally low when dealing with a lot of dice. This, however, can lead
#' to error magnification.
#'
#' In order to avoid rouding as much as possible, all functions described here
#' use [Ryacas::yac_str()] to run computations symbolically. By default,
#' results are converted to numeric vectors just before returning to the user,
#' but one is able to access the symbolic strings returned by Ryacas by setting
#' `precise = TRUE`.
#'
#' @seealso [roll], [roll-plot], [Dice].
#'
#' @param roll A roll expression (e.g., `2 * d6 + 5`).
#' @param precise Whether to return values with arbitrary precision.
#' @return A data frame with four columns: `outcome`, `n`, `d`, and `p`.
#'
#' @examples
#' # Get full distribution of 2d6 + 5
#' r(2 * d6 + 5)
#'
#' @export
r <- function(roll, precise = FALSE) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Convert values to numeric if requested
  if (!precise) {
    df$d <- yac_n(df$d)
    df$n <- yac_n(df$n)
  }

  return(df)
}

#' The roll distribution
#'
#' @description
#' Density, distribution function, quantile function, and random generation for
#' the discrete distribution described by a roll expression. See below for more
#' details.
#'
#' @details
#' Given a roll expression (i.e., an arithmetic expression involving dice),
#' [r()] calculates the complete distribution of the outcomes. This is possible
#' because the distribution is discrete and has a finite number of outcomes.
#'
#' From this distribution, [droll()] returns the density, [proll()] returns the
#' distribution function, [qroll()] returns the quantile function, and
#' [rroll()] generates random deviates. They mirror functions from the
#' [Distributions] family.
#'
#' For more details on roll expressions, see [r()] and the [Dice] S4 class.
#'
#' @seealso [r()], [Dice], [roll-plot].
#'
#' @source
#' The main algorithm for calculating dice probabilities comes from
#' [MathWorld](https://mathworld.wolfram.com/Dice.html).
#'
#' Symbolic calculations are handled by
#' [Ryacas](https://cran.r-project.org/package=Ryacas), and, by extension, by
#' [Yacas](https://www.yacas.org/).
#'
#' @param x A numeric vector of outcomes.
#' @param q A numeric vector of outcomes.
#' @param p A numeric vector of probabilities.
#' @param n Number of random deviates to return.
#' @param roll A roll expression (e.g., `2 * d6 + 5`) or a data frame returned
#' by [r()].
#' @param lower.tail Whether to calculate `P[X <= x]` or `P[X > x]`.
#' @param verbose For [rroll()], if `n = 1`, whether to print partial result of
#' the dice roll.
#' @return A numeric vector.
#'
#' @examples
#' set.seed(42)
#'
#' # Density of 2d6 + 5
#' droll(12, 2 * d6 + 5)
#'
#' # Distribution function of 2d6 + 5
#' proll(12, 2 * d6 + 5)
#'
#' # Quantile function of 2d6 + 5
#' qroll(0.5, 2 * d6 + 5)
#'
#' # Roll 2d6 + 5 (generate random deviates)
#' rroll(1, 2 * d6 + 5)
#'
#' @name roll
NULL

#' @rdname roll
#' @export
droll <- function(x, roll) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Filter correct frequencies and convert to numeric
  yac_n(df$d[df$outcome %in% x])
}

#' @rdname roll
#' @export
proll <- function(q, roll, lower.tail = TRUE) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  tails <- c()
  for (n in q) {

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

  return(tails)
}

#' @rdname roll
#' @export
qroll <- function(p, roll, lower.tail = TRUE) {

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

  return(tails)
}

#' @rdname roll
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
    out <- append(out, eval_dice(expr, parent.frame()))
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
