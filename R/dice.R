
#' Roll dice
#' @param sides Sides
#' @param n N
#' @export
d <- function(sides, n = 1) {
  sum(sample(1:sides, n, TRUE))
}

# Roll dice
`%d%` <- function(x, y) {
  d(x, y)
}

setClass(
  "Dice",
  contains = "numeric",
  slots = c(v = "numeric")
)

setGeneric("v", function(x) standardGeneric("v"))
methods::setMethod("v", "Dice", function(x) x@v)

setGeneric("roll", function(object, n = 1) standardGeneric("roll"))
methods::setMethod("roll", "Dice", function(object, n = 1) {
  sample(object@v, n, TRUE)
})

methods::setMethod("show", "Dice", function(object) {
  print(roll(object))
})

# setValidity("Dice", function(object) {
#   if (length(object@v) != length(object@w)) {
#     "@v and @w must be same length"
#   } else {
#     TRUE
#   }
# })

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
Dice <- function(faces) {
  methods::new("Dice", v = faces)
}

# c(d6, 2, 3, d6)
#
# c(d6, "1", d6)
#
# c(2, d6)

# d20kh <- Dice(1:20, filter = max)
#
# l <- purrr::rerun(1000, 2*d20kh)
# table(purrr::flatten_dbl(l))
#
# d20kl <- Dice(1:20, filter = min)
#
# l <- purrr::rerun(1000, 2*d20kl)
# table(purrr::flatten_dbl(l))
#
# dF <- Dice(-1:1)
#
# l <- purrr::rerun(1000, 1*dF) ### This is a problem (maybe solved with as())
# table(purrr::flatten_dbl(l))
