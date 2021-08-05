
#' Get the probability of passing or failing a check
#'
#' @description
#' These are simple functions that calculate probabilities of hits/misses and
#' passes/fails. They hide the complexities of the [roll] family of functions
#' so users unfamiliar with R's d/p/q/r notation can get quickly up and running
#' with the package.
#'
#' @details
#' This family of functions takes a roll expression (i.e., an arithmetic
#' expression involving dice) and calculates the probability of either
#' hitting/missing an attack or passing/failing a check. Since attacks and
#' checks work in the same way (i.e., roll a value higher than or equal to a
#' certain threshold), there are no `attack_*()` functions.
#'
#' For more details on roll expressions, see [r()] and the [Dice] S4 class.
#'
#' @seealso [roll], [r()], [Dice].
#'
#' @param roll A roll expression (e.g., `2 * d6 + 5`) or a data frame returned
#' by [r()].
#' @param dc The dificulty class to beat for a skill check. Conversely, the
#' armor class to beat for an attack.
#' @return A numeric scalar.
#'
#' @examples
#' # Probability of d20 + 8 passing a DC 15 skill check
#' check_pass(d20 + 8, 15)
#'
#' # Probability of d20 + 8 missing an AC 15 attack
#' check_fail(d20 + 8, 15)
#'
#' @name check

#' @rdname check
#' @export
check_pass <- function(roll, dc) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  tail <- df$d[df$outcome >= dc]
  yac_n(yac("Add", paste0(tail, collapse = ",")))
}

#' @rdname check
#' @export
check_fail <- function(roll, dc) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  tail <- df$d[df$outcome < dc]
  yac_n(yac("Add", paste0(tail, collapse = ",")))
}
