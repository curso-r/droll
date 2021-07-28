
#' Calculate how many ways each possible outcome of some dice can be obtained
#'
#' Given `n` dice with a numeric vector representing their `faces`, compute the
#' absolute frequency of each possible outcome of their sum. In other words,
#' calculate how many ways every outcome of 'NdF' can be obtained.
#'
#' @param faces A numeric vector with the dice's faces.
#' @param n The number of dice.
#' @return A list of lists of the form `list(outcome, count)`.
#'
#' @noRd
dice_outcome_count <- function(faces, n = 1) {

  # Polynomial expression for expansion (handled by Yacas)
  body <- paste0("(", paste0("x^", faces, collapse = "+"), ")^", n)
  poly <- yac("Expand", body)

  # Expanded polynomial expression and add trivial 1s)
  poly <- gsub("x(\\+|$)", "x^1\\1", gsub("(^|\\+)x", "\\11*x", poly))

  # Convert coeficients into outcome counts
  Map(
    function(l) list(outcome = as.numeric(l[2]), count = l[1]),
    strsplit(strsplit(poly, "\\+")[[1]], "\\*?x\\^")
  )
}

#' Check if expression belongs to the Dice S4 class
#'
#' @param expr An expression of class 'call'.
#' @param env The environment of `expr`.
#' @return A boolean.
#'
#' @noRd
is_die <- function(expr, env) {
  tryCatch(class(eval(expr, env)), error = function(e) "None") == "Dice"
}

#' Handle dice_outcome_count() for different kinds of expressions
#'
#' @param expr An expression of class 'call'.
#' @param env The environment of `expr`.
#' @param dice Whether `expr` is of the form NdF.
#' @return The output of [dice_outcome_count()]
#'
#' @noRd
get_doc <- function(expr, env, dice = FALSE) {

  # Evaluate and get DOC (if expression is of the form N * dF)
  if (dice) {
    return(dice_outcome_count(
      faces(eval(expr[[3]], env)),
      eval(expr[[2]], env))
    )
  }

  # Return DOC directly (if expression is a single die)
  return(dice_outcome_count(faces(eval(expr, env))))
}

#' Mask dice as inputs of a function (and get their dice_outcome_count())
#'
#' Given an expression with one Dice object, get its [dice_outcome_count()] and
#' replace it with `dots[[UUID]]` so that the expression can be used as the body
#' of a function.
#'
#' @param expr_and_counts A list with an expression and a count data frame.
#' @param env The environment of `expr_and_counts[[1]]`.
#' @param dice Whether `expr_and_counts[[1]]` is of the form NdF.
#' @return A list with an expression and a count data frame.
#'
#' @noRd
mask_dice <- function(expr_and_counts, env, dice = FALSE) {

  # Separate parts of input
  expr <- expr_and_counts[[1]]
  counts <- expr_and_counts[[2]]

  # Get dice_outcome_count() of expression
  doc <- get_doc(expr, env, dice)

  # Create a pseudo-UUID
  id <- paste0(sample(letters[1:16], 36, TRUE), collapse = "")

  # Transform expression into unique input and append DOC
  expr <- bquote(dots[[.(id)]])
  counts <- append(counts, stats::setNames(list(doc), id))

  return(list(expr, counts))
}

#' Mask all dice of a roll expression as inputs of a function
#'
#' Given an expression with one or more Dice objects, get their
#' [dice_outcome_count()] and replace them with `dots[[UUID]]` so that the
#' expression can be used as the body of a function.
#'
#' @param expr_and_counts A list with an expression and a count data frame.
#' @param env The environment of `expr_and_counts[[1]]`.
#' @return A list with an expression and a count data frame.
#'
#' @noRd
mask_roll <- function(expr_and_counts, env) {

  # Separate parts of input
  expr <- expr_and_counts[[1]]
  counts <- expr_and_counts[[2]]

  # Mask dice from LHS of expression
  if (is_die(expr[[2]], env)) {
    out <- mask_dice(list(expr[[2]], counts), env)
    expr[[2]] <- out[[1]]
    counts <- out[[2]]
  }

  # Handle parentheses and unary functions
  if (length(expr) == 2) {
    out <- mask_roll(list(expr[[2]], counts), env)
    expr[[2]] <- out[[1]]
    counts <- out[[2]]

    return(list(expr, counts))
  }

  # DFS to evaluate every branch of the expression
  idx <- which(sapply(expr, typeof) == "language")

  # Mask dice from RHS of expression
  if (is_die(expr[[3]], env)) {

    # Handle expressions of the form N * dS
    if (length(idx) == 0 && expr[[1]] == "*") {
      return(mask_dice(list(expr, counts), env, TRUE))
    }

    # Otherwise simply mask the dice
    out <- mask_dice(list(expr[[3]], counts), env)
    expr[[3]] <- out[[1]]
    counts <- out[[2]]
  }

  # Iterate over both branches of the expression
  for (i in idx) {
    out <- mask_roll(list(expr[[i]], counts), env)
    expr[[i]] <- out[[1]]
    counts <- out[[2]]
  }

  return(list(expr, counts))
}

#' Count the ways each outcome of a roll can be obtained (internal)
#'
#' Given a roll expression, compute the absolute frequency of each possible
#' outcome. In other words, calculate how many ways can every outcome of the
#' roll be obtained. [roll_outcome_count()] wraps this function so that the user
#' doesn't have to worry about environments.
#'
#' @param roll A roll expression (e.g. `2 * d6 + 5`).
#' @param precise Whether to return values with arbitrary precision.
#' @param env The environment of `roll`.
#' @return A data frame with two columns: `outcome` and `count`.
#'
#' @noRd
roll_outcome_count_ <- function(roll, precise = FALSE, env = parent.frame()) {

  # Capture roll expression and mask dice objects
  expr_and_counts <- mask_roll(list(roll, list()), env)
  expr <- expr_and_counts[[1]]
  counts <- expr_and_counts[[2]]

  # Create a function that inputs values in the roll formula (final outcome)
  roll_function <- function(...) {
    dots <- list(...)
    eval(expr, envir = list(dots))
  }

  # Transpose list of dice outcomes
  dice_out <- Map(
    function(l) do.call(Map, c(f = list, l)),
    tryCatch(cross(counts), error = function(e) stop("Invalid expression"))
  )

  # Actually roll dice with roll_function() and multiply outcomes' counts
  roll_out <- Map(function(l) {
    data.frame(
      outcome = do.call(roll_function, as.list(l$outcome)),
      count = yac("Multiply", paste0(l$count, collapse = ","))
    )
  }, dice_out)

  # Summarise table by outcome, adding counts
  df <- stats::aggregate(
    count ~ outcome, data = do.call(rbind, roll_out),
    function(l) yac("Add", paste0(l, collapse = ","))
  )

  # Convert values to numeric if requested
  if (!precise) {
    df$count <- as.numeric(df$count)
  }

  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(df))
  } else {
    return(df)
  }
}

#' Count the ways each outcome of a roll can be obtained
#'
#' Given a roll expression (i.e. an arithmetic expression involving dice),
#' compute the absolute frequency of each possible outcome. In other words,
#' calculate how many ways every outcome of the roll can be obtained.
#'
#' @param roll A roll expression (e.g. `2 * d6 + 5`).
#' @param precise Whether to return values with arbitrary precision.
#' @return A data frame with two columns: `outcome` and `count`.
#'
#' @examples
#' # Possible outcomes of 2d6 + 6
#' d6 <- d(1:6)
#' roll_outcome_count(2 * d6 + 5)
#' @export
roll_outcome_count <- function(roll, precise = FALSE) {
  roll_outcome_count_(substitute(roll), precise, parent.frame())
}
