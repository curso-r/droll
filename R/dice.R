
#' An S4 class to represent dice
#'
#' @description
#' A virtual representation of a die that supports the same arithmetic
#' operations as a numeric scalar, with the special property that, when
#' operated on, its value is randomly sampled from the die's faces. See below
#' for more details.
#'
#' @details
#' This S4 class extends [numeric] with the goal of creating an interactive die
#' inside of R. In short, an instance of this class functions as a numeric
#' scalar for most intents and purposes except that, when its value is needed,
#' it returns one of its faces at random.
#'
#' For more information on exactly what operations are supported, see the
#' **Operations** section below. To learn more about how to create an object of
#' this class, see the dice creating function [d()]. For roll distributions,
#' see the [roll] family. For plotting those distributions, see the [roll-plot]
#' family.
#'
#' @section Operations:
#' By default, when printed, an object of this class returns a numeric vector
#' with all of its faces. In order to actually "roll" the die (that is, get one
#' of its faces at random), one can simply operate on it. Any arithmetic
#' expression should trigger a die into sampling its faces, even `dX + 0` and
#' `1 * dX`.
#'
#' All standard arithmetic operations are supported, along with comparisons,
#' logic assertions, mathematical functions, and summaries: every group
#' described in [S4groupGeneric] except for `Complex`. Note that, when used in
#' other situations, like [c()], the die will return all of its faces.
#'
#' These functions also work in the exact same way as they would with regular
#' numeric scalars, with the exception of multiplication. With the goal of
#' supporting the very common operation `NdX` ("rolling `N` dice with `X` faces
#' and adding the results"), the multiplication symbol behaves differently
#' depending on the context: `N * dX` will work as `NdX` and `dX * N` will work
#' as `N x dX` ("rolling a die with `X` faces and multiplying the result by
#' `N`").
#'
#' The [roll] and [roll-plot] families of functions make ample use of roll
#' expressions like the ones described here. They even support some built-in
#' dice that can be used without being created with [d()].
#'
#' @seealso [d()], [roll], [roll-plot].
#'
#' @slot faces A numeric vector with the die's faces.
#'
#' @examples
#' set.seed(42)
#'
#' # Create some dice with d()
#' d6 <- d(6)
#' d20 <- d(20)
#'
#' # Print faces from die
#' d6
#'
#' # Roll 1d6
#' 1 * d6
#'
#' # Check if an attack hits and deal damage
#' if (d20 + 8 >= 12) {
#'   print(4 * d6)
#' } else {
#'   print(0)
#' }
#' @name Dice
methods::setClass(
  "Dice",
  contains = "numeric",
  slots = c(faces = "numeric")
)

#' Create a die
#'
#' @description
#' Create an instance of the [Dice] S4 class, allowing for the use of
#' non-standard dice and for interactive dice rolling without having to recur
#' to functions of the [roll] family. See below for more details.
#'
#' @details
#' If given a numeric vector, [d()] creates an object of the [Dice] S4 class
#' representing a die with these values for faces. On the other hand, if given
#' a numeric scalar, it creates a die with faces running from 1 to this value.
#' At the moment, there is no support for specifying each face's probability,
#' although it is possible to create a die where more than one face have the
#' same value.
#'
#' This function has two main purposes: creating non-standard dice and allowing
#' for interactive dice rolling. Non-standard dice are arbitrary objects that
#' might not have a real world counterpart, e.g., a `d(17)` or a
#' `d(c(1, 1, 3))`. Interactive rolling is the ability to get a random result
#' from a die without having to resort to functions of the [roll] family,
#' explained in detail in the documentation for the [Dice] S4 class.
#'
#' @seealso [Dice], [roll].
#'
#' @param faces Either the number of faces (`length(faces) == 1`) or a numeric
#' vector specifying the values of the faces.
#' @return An object of the [Dice] S4 class.
#'
#' @examples
#' # Create a d6
#' d6 <- d(6)
#' d6
#'
#' # Create a die with even faces
#' dEven <- d(c(2, 4, 6))
#' dEven
#'
#' # Create a loaded die
#' dLoaded <- d(c(1:6, 6))
#' dLoaded
#' @export
d <- function(faces) {

  # Shortcut for d(X)
  if (length(faces) == 1 && faces >= 1) {
    faces <- 1:faces
  }

  methods::new("Dice", faces, faces = faces)
}

#' Manipulate faces of a Dice object
#'
#' @param x A Dice object.
#'
#' @noRd
setGeneric("faces", function(x) standardGeneric("faces"))

#' @describeIn faces Get faces of a Dice object.
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

#' Simulate rolling dice
#'
#' @param object A Dice object.
#' @param n Number of dice.
#'
#' @noRd
setGeneric("s", function(object, n = 1) standardGeneric("s"))

#' @describeIn r Roll a Dice object.
#' @noRd
methods::setMethod("s", "Dice", function(object, n = 1) {
  return(as.numeric(sample(faces(object), n, TRUE)))
})

#' Print a Dice object
#'
#' @param object A Dice object.
#'
#' @rdname Dice
methods::setMethod("show", "Dice", function(object) {
  cat("\033[38;5;246m# A die with faces:\n\033[39m")
  print(faces(object))
})

#' Arithmetic operations with Dice objects
#'
#' @param e1 A numeric scalar or a Dice object.
#' @param e2 A numeric scalar or a Dice object.
#'
#' @rdname Dice
methods::setMethod("Ops", c("Dice", "numeric"), function(e1, e2) {
  methods::callGeneric(s(e1), e2)
})

#' Arithmetic operations with Dice objects
#'
#' @param e1 A numeric scalar or a Dice object.
#' @param e2 A numeric scalar or a Dice object.
#'
#' @rdname Dice
methods::setMethod("Ops", c("numeric", "Dice"), function(e1, e2) {
  methods::callGeneric(e1, s(e2))
})

#' Arithmetic operations with Dice objects
#'
#' @param e1 A numeric scalar or a Dice object.
#' @param e2 A numeric scalar or a Dice object.
#'
#' @rdname Dice
methods::setMethod("Ops", c("Dice", "Dice"), function(e1, e2) {
  methods::callGeneric(s(e1), s(e2))
})

#' Arithmetic operations with Dice objects
#'
#' @param e1 A numeric scalar or a Dice object.
#' @param e2 A numeric scalar or a Dice object.
#'
#' @rdname Dice
methods::setMethod("*", c("numeric", "Dice"), function(e1, e2) {
  sum(s(e2, e1))
})

#' Math operations with Dice objects
#'
#' @param x A Dice object.
#'
#' @rdname Dice
methods::setMethod("Math", "Dice", function(x) {
  methods::callGeneric(s(x))
})

#' Math2 operations with Dice objects
#'
#' @param x A Dice object.
#' @param digits Number of digits to be used in [round()] or [signif()].
#'
#' @rdname Dice
methods::setMethod("Math2", "Dice", function(x, digits) {
  methods::callGeneric(s(x), digits)
})

#' Summary operations with Dice objects
#'
#' @param x A Dice object
#' @param ... Numeric arguments.
#' @param na.rm A logical indicating whether missing values should be removed.
#'
#' @rdname Dice
methods::setMethod("Summary", "Dice", function(x, ..., na.rm = FALSE) {
  methods::callGeneric(s(x), as.numeric(...), na.rm = na.rm)
})
