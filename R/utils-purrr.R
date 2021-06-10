
# Copy from purrr::cross()
cross <- function (.l)  {
  n <- length(.l)
  lengths <- lapply(.l, length)
  names <- names(.l)
  factors <- cumprod(lengths)
  total_length <- factors[n]
  factors <- c(1, factors[-n])
  out <- replicate(total_length, vector("list", n), simplify = FALSE)
  for (i in seq_along(out)) {
    for (j in seq_len(n)) {
      index <- floor((i - 1)/factors[j])%%length(.l[[j]]) +
        1
      out[[i]][[j]] <- .l[[j]][[index]]
    }
    names(out[[i]]) <- names
  }
  out
}
