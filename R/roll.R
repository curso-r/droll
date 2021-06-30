
#' Roll some dice
#'
#' Given a roll expression (i.e. an arithmetic expression involving dice),
#' compute an outcome by sampling one face from each die and evaluating the
#' expression. If `verbose = TRUE`, also print the expression before evaluation.
#'
#' @param roll A roll expression (e.g. `2 * d6 + 5`).
#' @param verbose Whether to print the expression after the dice rolls.
#' @return A numeric scalar.
#'
#' @examples
#' # Roll 2d6 + 6
#' d6 <- d(1:6)
#' roll(2 * d6 + 5)
#'
#' @export
roll <- function(roll, verbose = TRUE) {

  # BFS on the expression tree, evaluating dice rolls
  roll_dice <- function(expr, env) {

    # Simplest expression, either roll or just eval
    if (length(expr) == 1) {
      if (is_die(expr, env)) {
        return(r(eval(expr, env)))
      } else {
        return(eval(expr, env))
      }
    }

    # Evaluate argument of unary functions
    if (length(expr) == 2) {
      expr[[2]] <- roll_dice(expr[[2]], env)
      return(expr)
    }

    # Bypass regular `*` roll in order to get partial results
    # TODO: not a really good solution, try to not bypass
    if (expr[[1]] == "*" && is_die(expr[[3]], env) && !is_die(expr[[2]], env)) {

      # Backport of str2lang() for R 3.6 and older
      if (R.version$major < 4) {
        str2lang <- function(s) {
          parse(text = s, keep.source = FALSE)[[1]]
        }
      }

      dice <- r(eval(expr[[3]], env), eval(expr[[2]], env))
      return(str2lang(paste0("sum(", paste(dice, collapse = " + "), ")")))
    }

    # Evaluate both sides of binary operations
    expr[[2]] <- roll_dice(expr[[2]], env)
    expr[[3]] <- roll_dice(expr[[3]], env)
    return(expr)
  }

  # Roll dice making each result explicit
  expr <- roll_dice(substitute(roll), parent.frame())

  # Print expression for user befor evaluating
  if (verbose) {
    cat(
      "\033[38;5;246m# Outcome:",
      gsub(" +", " ", paste0(deparse(expr), collapse = "")),
      "\n\033[39m"
    )
  }

  # Eval expression
  eval(expr, parent.frame())
}
