
#' Get the probability of passing a check or hitting an attack
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
#' hitting/missing an attack or passing/failing a check. There are also
#' functions to calculate the probability of a critical hit/miss and of a
#' critical pass/fail.
#'
#' Note that a critical success is considered to occur when the given die
#' rolls its highest possible value and that a critical fumble occurs when
#' opposite happens.
#'
#' For more details on roll expressions, see [r()] and the [Dice] S4 class.
#'
#' @seealso [roll], [r()], [Dice].
#'
#' @param roll A roll expression (e.g., `2 * d6 + 5`) or a data frame returned
#' by [r()].
#' @param ac The armor class to beat for an attack.
#' @param dc The dificulty class to beat for a skill check.
#' @return A numeric scalar.
#'
#' @examples
#' # Probability of d20 + 8 passing a DC 15 skill check
#' check_pass(d20 + 8, 15)
#'
#' # Probability of d20 + 8 missing an AC 15
#' attack_miss(d20 + 8, 15)
#'
#' # Probability of disadvantaged roll critting
#' attack_critical_hit(min(d20, d20))
#'
#' @name attack-check

#' @rdname attack-check
#' @export
attack_hit <- function(roll, ac) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  tail <- df$d[df$outcome >= ac]
  yac_n(yac("Add", paste0(tail, collapse = ",")))
}

#' @rdname attack-check
#' @export
attack_miss <- function(roll, ac) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  tail <- df$d[df$outcome < ac]
  yac_n(yac("Add", paste0(tail, collapse = ",")))
}

#' @rdname attack-check
#' @export
attack_critical_hit <- function(roll) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  yac_n(utils::tail(df, 1)$d)
}

#' @rdname attack-check
#' @export
attack_critical_miss <- function(roll) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  yac_n(utils::head(df, 1)$d)
}

#' @rdname attack-check
#' @export
check_pass <- function(roll, dc) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  tail <- df$d[df$outcome >= dc]
  yac_n(yac("Add", paste0(tail, collapse = ",")))
}

#' @rdname attack-check
#' @export
check_fail <- function(roll, dc) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  tail <- df$d[df$outcome < dc]
  yac_n(yac("Add", paste0(tail, collapse = ",")))
}

#' @rdname attack-check
#' @export
check_critical_pass <- function(roll) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  yac_n(utils::tail(df, 1)$d)
}

#' @rdname attack-check
#' @export
check_critical_fail <- function(roll) {

  # Get full distribution
  df <- roll_outcome_count(substitute(roll), parent.frame())

  # Get tail of the distribution
  yac_n(utils::head(df, 1)$d)
}