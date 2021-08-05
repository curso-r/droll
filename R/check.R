
#' Get the probability of passing or failing a check
#'
#' @description
#' This is a simple function that calculates probabilities of hits/misses and
#' passes/fails. It hides the complexities of the [roll] family of functions so
#' users unfamiliar with R's d/p/q/r notation can get quickly up and running
#' with the package.
#'
#' @details
#' This function takes a roll expression (i.e., an arithmetic expression
#' involving dice) and calculates the probability of either hitting/missing an
#' attack or passing/failing a check. Since attacks and checks work in the same
#' way (i.e., roll a value higher than or equal to a certain threshold), there
#' is no `attack_hit()` function.
#'
#' For more details on roll expressions, see [r()] and the [Dice] S4 class.
#'
#' @seealso [roll], [r()], [Dice].
#'
#' @param roll A roll expression (e.g., `2 * d6 + 5`) or a data frame returned
#' by [r()].
#' @param dc The dificulty class to beat for a skill check (or the armor class
#' to beat for an attack).
#' @param pass Whether to calculate the probability of passing the check (the
#' default) or failing it.
#' @return A numeric scalar.
#'
#' @examples
#' # Probability of d20 + 8 passing a DC 15 skill check
#' check_prob(d20 + 8, 15)
#'
#' # Probability of d20 + 8 missing an AC 15 attack
#' check_prob(d20 + 8, 15, pass = FALSE)
#'
#' @export
check_prob <- function(roll, dc, pass = TRUE) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Handle 0
  if (dc > max(df$outcome) || dc < min(df$outcome)) {
    return(0)
  }

  # Create a reverse p vector (P[X >= x])
  df$p <- rev(Reduce(
    function(x, y) yac("Add", paste0(x, ",", y)),
    rev(df$d), accumulate = TRUE
  ))

  # Get tail of the distribution
  tail <- df$p[min(which(df$outcome >= dc))]

  # Handle side of tail
  if (!pass) {
    tail <- paste0("1 - ", tail)
  }

  return(yac_n(tail))
}

#' Get the DC of a check given its success/failure probability
#'
#' @description
#' This is a simple function that calculates the required difficulty class so
#' that a skill check has the given success/failure probability. It hides the
#' complexities of the [roll] family of functions so users unfamiliar with R's
#' d/p/q/r notation can get quickly up and running with the package.
#'
#' @details
#' This function takes a roll expression (i.e., an arithmetic expression
#' involving dice) and calculates the DC of a skill check with a certain
#' success/failure probability. Since attacks and checks work in the same
#' way (i.e., roll a value higher than or equal to a certain threshold), there
#' is no `attack_dc()` function.
#'
#' For more details on roll expressions, see [r()] and the [Dice] S4 class.
#'
#' @seealso [roll], [r()], [Dice].
#'
#' @param roll A roll expression (e.g., `2 * d6 + 5`) or a data frame returned
#' by [r()].
#' @param p The probability of success/failure of the check (or attack).
#' @param pass Whether to calculate the DC necessary for passing the check (the
#' default) or failing it.
#' @return A numeric scalar.
#'
#' @examples
#' # DC of a check so player has (at least) a 50% chance of success
#' check_dc(d20 + 8, 0.5)
#'
#' # DC of a check so player has (at most) a 30% chance of failure
#' check_dc(d20 + 8, 0.3, pass = FALSE)
#'
#' @export
check_dc <- function(roll, p, pass = TRUE) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Create a reverse p vector (P[X >= x])
  df$p <- rev(Reduce(
    function(x, y) yac("Add", paste0(x, ",", y)),
    rev(df$d), accumulate = TRUE
  ))

  # Convert to numeric
  df$p <- yac_n(df$p)

  # Get tail of the distribution
  if (pass) {
    tail <- df$outcome[max(which(df$p >= p))]
  } else {
    tail <- df$outcome[max(which(1 - df$p <= p))]
  }

  return(tail)
}
