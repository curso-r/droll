
# Global regex for matching dice
dice_regex <- "([0-9]*?)[dD]([0-9]+)"

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
  slots = c(
    v = "numeric",
    w = "numeric",
    f = "function"
  )
)

setGeneric("v", function(x) standardGeneric("v"))
setMethod("v", "Dice", function(x) x@v)

setGeneric("w", function(x) standardGeneric("w"))
setMethod("w", "Dice", function(x) x@w)

setGeneric("f", function(x) standardGeneric("f"))
setMethod("f", "Dice", function(x) x@f)

setGeneric("roll", function(object, n = 1) standardGeneric("roll"))
setMethod("roll", "Dice", function(object, n = 1) {
  object@f(sample(object@v, n, TRUE, object@w))
})

setMethod("show", "Dice", function(object) {
  print(roll(object))
})

# setValidity("Dice", function(object) {
#   if (length(object@v) != length(object@w)) {
#     "@v and @w must be same length"
#   } else {
#     TRUE
#   }
# })

setMethod("Ops", c("Dice", "numeric"), function(e1, e2) {
  methods::callGeneric(roll(e1), e2)
})

setMethod("Ops", c("numeric", "Dice"), function(e1, e2) {
  methods::callGeneric(e1, roll(e2))
})

setMethod("Ops", c("Dice", "Dice"), function(e1, e2) {
  methods::callGeneric(roll(e1), roll(e2))
})

#' Roll multiple dice and add outcomes
#' @param e1 A numeric value
#' @param e2 A Dice object
#'
setMethod("*", c("numeric", "Dice"), function(e1, e2) {
  sum(roll(e2, e1))
})

#' Roll multiple dice and multiply outcomes
#' @param e1 A numeric value
#' @param e2 A Dice object
#'
setMethod("^", c("numeric", "Dice"), function(e1, e2) {
  prod(roll(e2, e1))
})

#' Create a Dice object
#' @param faces A numeric vector
#' @param weights A numeric vector
#' @param filter A function
#' @export
Dice <- function(faces, weights = rep(1/length(faces), length(faces)), filter = function(x) x) {
  methods::new("Dice", v = faces, w = weights, f = filter)
}




# d6 <- Dice(1:6)
#
# d6
#
# 10 * d6
# d6 * 10
#
# 10 + d6
# d6 + 10
#
# 10 / d6
# d6 / 10
#
# 10 - d6
# d6 - 10
#
# 10 ^ d6
# d6 ^ 10
#
# 10 %% d6
# d6 %% 10
#
# 10 %/% d6
# d6 %/% 10

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
# dF <- Dice(-1:1, rep(1/3, 3))
#
# l <- purrr::rerun(1000, 1*dF) ### This is a problem (maybe solved with as())
# table(purrr::flatten_dbl(l))





