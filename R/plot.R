
# counts <-
# counts <- roll_outcome_count(40 * d6)
#
# barplot(count ~ outcome, data = counts)

plot_outcome_count <- function(roll, env = parent.frame()) {
  roll_outcome_count(substitute(roll))
}

# d6 <- alea::d(1:6)
# alea::roll_outcome_count(40 * d6)
#
# roll_outcome_count()
