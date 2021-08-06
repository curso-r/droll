
#' Work with skill checks' probabilities and DCs
#'
#' @description
#' These are simple functions that manipulate probabilities and DCs for skill
#' checks. [check_prob()] calculates the success/failure probability of a check
#' with the given DC. [check_dc()] calculates the required difficulty class so
#' that a skill check has the given success/failure probability. See below for
#' more details.
#'
#' @details
#' These functions hide the complexities of the [roll] family so users
#' unfamiliar with R's d/p/q/r notation can get quickly up and running
#' with the package. Since attacks and checks work in the same way (i.e.,
#' success means rolling a value higher than or equal to a certain threshold),
#' there are no `attack_*()` functions.
#'
#' For more details on roll expressions, see [r()] and the [Dice] S4 class.
#'
#' @seealso [roll], [r()], [Dice].
#'
#' @param roll A roll expression (e.g., `2 * d6 + 5`) or a data frame returned
#' by [r()].
#' @param p The probability of success/failure of the check (or attack).
#' @param dc The difficulty class to beat for a skill check (or the armor class
#' to beat for an attack).
#' @param success Whether to aim for success (the default) or for failure on
#' the check (or attack).
#' @return A numeric scalar.
#'
#' @examples
#' # Probability of d20 + 8 passing a DC 15 skill check
#' check_prob(d20 + 8, 15)
#'
#' # Probability of d20 + 8 missing an AC 15 attack
#' check_prob(d20 + 8, 15, success = FALSE)
#' @name check

#' @rdname check
#' @export
check_dc <- function(roll, p, success = TRUE) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Create a reverse p vector (P[X >= x])
  df$p <- rev(Reduce(
    function(x, y) yac("Add", paste0(x, ",", y)),
    rev(df$d),
    accumulate = TRUE
  ))

  # Convert to numeric
  df$p <- yac_n(df$p)

  # Get tail of the distribution
  if (success) {
    tail <- df$outcome[max(which(df$p >= p))]
  } else {
    tail <- df$outcome[max(which(1 - df$p <= p))]
  }

  return(tail)
}

#' @rdname check
#' @export
check_prob <- function(roll, dc, success = TRUE) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Handle 0
  if (dc > max(df$outcome) || dc < min(df$outcome)) {
    return(0)
  }

  # Create a reverse p vector (P[X >= x])
  df$p <- rev(Reduce(
    function(x, y) yac("Add", paste0(x, ",", y)),
    rev(df$d),
    accumulate = TRUE
  ))

  # Get tail of the distribution
  tail <- df$p[min(which(df$outcome >= dc))]

  # Handle side of tail
  if (!success) {
    tail <- paste0("1 - ", tail)
  }

  return(yac_n(tail))
}
