
plot_outcome_count <- function(roll, ...) {
  counts <- roll_outcome_count_(substitute(roll), parent.frame())
  graphics::barplot(count ~ outcome, data = counts, ...)
}
