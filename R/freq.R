
#' Calculate the frequency with which each outcome of a roll can be obtained
#'
#' Given a roll expression (i.e. an arithmetic expression involving dice),
#' compute the relative frequency of each possible outcome. In other words,
#' calculate how many ways every outcome of the roll can be obtained and then
#' divide by the total sum.
#'
#' @param roll A roll expression (e.g. `2 * d6 + 5`).
#' @param precise Whether to return values with arbitrary precision.
#' @return A data frame with two columns: `outcome` and `freq`.
#'
#' @examples
#' # Possible outcomes of 2d6 + 5
#' d6 <- d(1:6)
#' roll_outcome_freq(2 * d6 + 5)
#' @export
roll_outcome_freq <- function(roll, precise = FALSE) {

  # Calculate outcome count and sum total counts
  df <- roll_outcome_count_(substitute(roll), TRUE, parent.frame())
  t <- yac("Add", paste0(df$count, collapse = ","))

  # Calculate relative frequency with arbitrary precision
  df$freq <- sapply(df$count, function(n) yac("", paste0(n, "/", t)))

  # Clean columns
  names(df$freq) <- NULL
  df$count <- NULL

  # Convert values to numeric if requested
  if (!precise) {
    df$freq <- sapply(df$freq, function(f) yac("N", f))
    df$freq <- as.numeric(df$freq)
  }

  return(df)
}