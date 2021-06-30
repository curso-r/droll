
#' Simple infix dice roller
#'
#' @param n Number of dice.
#' @param sides Number of sides.
#' @return A numeric scalar.
#'
#' @examples
#' # Roll 2d6
#' 2 %d% 6
#'
#' @export
`%d%` <- function(n, sides) {
  sample(1:sides, n, TRUE)
}

#' An S4 class to represent dice
#'
#' @slot faces A numeric vector with the die's faces.
#'
methods::setClass(
  "Dice",
  contains = "numeric",
  slots = c(faces = "numeric")
)

#' Create a Dice object
#'
#' @param faces A numeric vector with the die's faces.
#' @return A Dice object.
#'
#' @examples
#' # Create a d6
#' d6 <- d(1:6)
#'
#' @export
d <- function(faces) {
  methods::new("Dice", faces, faces = faces)
}

#' Manipulate faces of a Dice object
#'
#' @param x A Dice object.
#'
#' @noRd
setGeneric("faces", function(x) standardGeneric("faces"))

#' @describeIn faces Get faces of a Dice object.
#'
#' @noRd
methods::setMethod("faces", "Dice", function(x) x@faces)

#' Validate a Dice object
#'
#' @param object A Dice object.
#'
#' @noRd
methods::setValidity("Dice", function(object) {
  if (!is.vector(object@faces, "numeric")) {
    "@faces must be a numeric vector"
  } else {
    TRUE
  }
})

#' Simulate rolling a die
#'
#' @param object A Dice object.
#' @param n Number of dice.
#'
#' @noRd
setGeneric("r", function(object, n = 1) standardGeneric("r"))

#' @describeIn r Roll a Dice object.
#'
#' @noRd
methods::setMethod("r", "Dice", function(object, n = 1) {
  return(as.numeric(sample(faces(object), n, TRUE)))
})

#' Print a Dice object
#'
#' @param object A Dice object.
#'
methods::setMethod("show", "Dice", function(object) {
  cat("\033[38;5;246m# A die with faces:\n\033[39m")
  print(faces(object))
})

#' Arithmetic operations with Dice objects
#'
#' @param e1 A numeric scalar or a Dice object.
#' @param e2 A numeric scalar or a Dice object.
#'
#' @name Ops-Dice
NULL

#' @rdname Ops-Dice
methods::setMethod("Ops", c("Dice", "numeric"), function(e1, e2) {
  methods::callGeneric(r(e1), e2)
})

#' @rdname Ops-Dice
methods::setMethod("Ops", c("numeric", "Dice"), function(e1, e2) {
  methods::callGeneric(e1, r(e2))
})

#' @rdname Ops-Dice
methods::setMethod("Ops", c("Dice", "Dice"), function(e1, e2) {
  methods::callGeneric(r(e1), r(e2))
})

#' @rdname Ops-Dice
methods::setMethod("*", c("numeric", "Dice"), function(e1, e2) {
  sum(r(e2, e1))
})

#' Coerce Dice object to numeric scalar by simulating a roll
#'
#' @param x A Dice object.
#' @param ... Not used.
#'
as.numeric.Dice <- function(x, ...) r(x)

#' @describeIn as.numeric.Dice S4 port
methods::setMethod("as.numeric", "Dice", as.numeric.Dice)

#' Combine Dice objects into a vector by simulating rolls
#'
#' @param x A Dice object.
#' @param ... Objects to be concatenated.
#' @param recursive Recursively descend.
#' @return A numeric vector.
#'
methods::setMethod("c", "Dice", function(x, ..., recursive = FALSE) {
  c(as.numeric(x), c(..., recursive = recursive))
})
