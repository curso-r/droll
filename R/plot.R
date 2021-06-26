
plot_outcome_count <- function(roll, ...) {
  counts <- roll_outcome_count_(substitute(roll), parent.frame())
  graphics::barplot(
    counts$count, names.arg = counts$outcome,
    xlab = "outcome", ylab = "count"
  )
}
