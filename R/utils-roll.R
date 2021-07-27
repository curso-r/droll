
#' Backport of str2lang() for R 3.6 and older
#'
#' @param s A character vector of length 1, i.e., a "string".
#' @return A `call` or simpler.
#'
#' @noRd
str2lang_ <- function(s) {
  if (R.version$major < 4) {
    parse(text = s, keep.source = FALSE)[[1]]
  } else {
    str2lang(s)
  }
}
