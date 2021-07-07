
#' Produce all combinations of list elements
#'
#' Take a list `.l` and return the cartesian product of all its elements in a
#' list, with one combination by element. This function was ported from the
#' `{purrr}` package in order to avoid extra dependencies.
#'
#' @param .l A list of lists or atomic vectors. Alternatively, a data frame.
#' @return A list where each element is one combination so that the list can be
#' directly mapped over.
#'
#' @noRd
cross <- function(.l) {
  n <- length(.l)
  lengths <- lapply(.l, length)
  names <- names(.l)
  factors <- cumprod(lengths)
  total_length <- factors[n]
  factors <- c(1, factors[-n])
  out <- replicate(total_length, vector("list", n), simplify = FALSE)
  for (i in seq_along(out)) {
    for (j in seq_len(n)) {
      index <- floor((i - 1) / factors[j]) %% length(.l[[j]]) + 1
      out[[i]][[j]] <- .l[[j]][[index]]
    }
    names(out[[i]]) <- names
  }
  out
}
