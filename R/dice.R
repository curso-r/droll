
# Roll dice
`%d%` <- function(x, y) {
  sample(1:y, x, TRUE)
}

methods::setClass(
  "Dice",
  contains = "numeric",
  slots = c(faces = "numeric")
)

methods::setGeneric("faces", function(x) standardGeneric("faces"))
methods::setMethod("faces", "Dice", function(x) x@faces)

methods::setGeneric("roll", function(object, n = 1) standardGeneric("roll"))
methods::setMethod("roll", "Dice", function(object, n = 1) {
  sample(object@faces, n, TRUE)
})

methods::setMethod("show", "Dice", function(object) {
  cat("\033[38;5;246m# A dice with faces:\n\033[39m")
  print(object@faces)
})

methods::setValidity("Dice", function(object) {
  if (!is.vector(object@faces, "numeric")) {
    "@faces must be a numeric vector"
  } else {
    TRUE
  }
})

methods::setMethod("Ops", c("Dice", "numeric"), function(e1, e2) {
  methods::callGeneric(roll(e1), e2)
})

methods::setMethod("Ops", c("numeric", "Dice"), function(e1, e2) {
  methods::callGeneric(e1, roll(e2))
})

methods::setMethod("Ops", c("Dice", "Dice"), function(e1, e2) {
  methods::callGeneric(roll(e1), roll(e2))
})

#' Roll multiple dice and add outcomes
#' @param e1 A numeric value
#' @param e2 A Dice object
#'
methods::setMethod("*", c("numeric", "Dice"), function(e1, e2) {
  sum(roll(e2, e1))
})

#' Create a Dice object
#' @param faces A numeric vector
#' @export
d <- function(faces) {
  methods::new("Dice", faces = faces)
}


#' Roll a dice
#' @param x A Dice object
#' @param ... Not used
#' @export
as.numeric.Dice <- function(x, ...) roll(x)

#' Roll a dice
#' @param x A Dice object
#' @param ... Not used
#' @export
methods::setMethod("as.numeric", "Dice", as.numeric.Dice)

#' Combine dice
#' @param x A Dice object
#' @param ... Objects to concatenate
#' @param recursive Recursively descend
#' @export
methods::setMethod("c", "Dice", function(x, ..., recursive = FALSE) {
  c(as.numeric(x), c(..., recursive = recursive))
})
