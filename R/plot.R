
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
#' plot_outcome_count(2 * d6 + 5)
#' @export
plot_outcome_count <- function(roll, ...) {
  counts <- roll_outcome_count(substitute(roll), env = parent.frame())
  graphics::barplot(
    counts$count,
    names.arg = counts$outcome,
    xlab = "outcome", ylab = "count", ...
  )
}
