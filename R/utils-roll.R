
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

#' Shortcut to [Ryacas::yac_str()]
#'
#' @param f Yacas function.
#' @param b Body of `f`.
#' @return A string with Yacas' output.
#'
#' @noRd
yac <- function(f, b) {
  Ryacas::yac_str(paste0(f, "(", b, ")"))
}

#' Convert strings with arbitrary precision to numerics
#'
#' @param x A character vector.
#' @return A numeric vector.
#'
#' @noRd
yac_n <- function(x) {
  if (is.numeric(x)) {
    return(x)
  } else {
    return(as.numeric(sapply(x, function(a) yac("N", a))))
  }
}

#' BFS on the expression tree, evaluating dice rolls
#'
#' @param expr An expression of class 'call'.
#' @param env The environment of `expr`.
#' @return An expression that can be evaluated by R.
#'
#' @noRd
roll_dice <- function(expr, env) {

  # Simplest expression, either roll or just eval
  if (length(expr) == 1) {
    if (is_die(expr, env)) {
      return(s(eval_dice(expr, env)))
    } else {
      return(eval(expr, env))
    }
  }

  # Evaluate argument of unary functions
  if (length(expr) == 2) {
    expr[[2]] <- roll_dice(expr[[2]], env)
    return(expr)
  }

  # Bypass regular `*` roll in order to get partial results
  # TODO: not a really good solution, try to not bypass
  if (expr[[1]] == "*" && is_die(expr[[3]], env) && !is_die(expr[[2]], env)) {
    dice <- s(eval_dice(expr[[3]], env), eval(expr[[2]], env))
    return(str2lang_(paste0("sum(", paste(dice, collapse = ", "), ")")))
  }

  # Evaluate both sides of binary operations
  expr[[2]] <- roll_dice(expr[[2]], env)
  expr[[3]] <- roll_dice(expr[[3]], env)
  return(expr)
}

#' Calculate how many ways each possible outcome of some dice can be obtained
#'
#' Given `n` dice with a numeric vector representing their `faces`, compute the
#' absolute frequency of each possible outcome of their sum. In other words,
#' calculate how many ways every outcome of 'NdX' can be obtained.
#'
#' @param faces A numeric vector with the dice's faces.
#' @param n The number of dice.
#' @return A list of lists of the form `list(outcome, n)`.
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
    function(l) list(outcome = as.numeric(l[2]), n = l[1]),
    strsplit(strsplit(poly, "\\+")[[1]], "\\*?x\\^")
  )
}

#' Wrapper around eval() that handles built-in dice
#'
#' @param expr An expression of class 'call'.
#' @param env The environment of `expr`.
#' @return The result of evaluating the expression.
#'
#' @noRd
eval_dice <- function(expr, env) {

  # Error handling funtion
  err <- function(e) {
    text <- as.character(expr)

    # Return a new dice if built-in
    if (length(text) == 1) {
      if (grepl("^[dD](4|6|8|10|12|20|100)$", text)) {
        return(d(sub("[dD]", "", text)))
      } else {
        stop(e)
      }
    }
  }

  # Try to evaluate
  tryCatch(eval(expr, env), error = err)
}

#' Check if expression belongs to the Dice S4 class
#'
#' @param expr An expression of class 'call'.
#' @param env The environment of `expr`.
#' @return A boolean.
#'
#' @noRd
is_die <- function(expr, env) {
  tryCatch(class(eval_dice(expr, env)), error = function(e) "None") == "Dice"
}

#' Mask dice as inputs of a function (and get their dice_outcome_count())
#'
#' Given an expression with one Dice object, get its [dice_outcome_count()] and
#' replace it with `dots[[UUID]]` so that the expression can be used as the body
#' of a function.
#'
#' @param expr_and_counts A list with an expression and a count data frame.
#' @param env The environment of `expr_and_counts[[1]]`.
#' @param dice Whether `expr_and_counts[[1]]` is of the form NdX.
#' @return A list with an expression and a count data frame.
#'
#' @noRd
mask_dice <- function(expr_and_counts, env, dice = FALSE) {

  # Separate parts of input
  expr <- expr_and_counts[[1]]
  counts <- expr_and_counts[[2]]

  # Handle dice_outcome_count() for different kinds of expressions
  get_doc <- function(expr, env, dice = FALSE) {

    # Evaluate and get DOC (if expression is of the form N * dX)
    if (dice) {
      return(dice_outcome_count(
        faces(eval_dice(expr[[3]], env)),
        eval(expr[[2]], env)
      ))
    }

    # Return DOC directly (if expression is a single die)
    return(dice_outcome_count(faces(eval_dice(expr, env))))
  }

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

  # Handle single die expressions
  if (length(expr) == 1) {
    if (is_die(expr, env)) {
      out <- mask_dice(list(expr, counts), env)
      return(list(out[[1]], out[[2]]))
    }

    # If there are no dice, just return
    return(list(expr, counts))
  }

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

#' Count the ways each outcome of a roll can be obtained
#'
#' Given a roll expression, compute the absolute frequency of each possible
#' outcome. In other words, calculate how many ways can every outcome of the
#' roll be obtained. [r()] wraps this function so that the user doesn't have to
#' worry about environments.
#'
#' @param roll A roll expression (e.g., `2 * d6 + 5`) or a data frame returned
#' by [r()].
#' @param env The environment of `roll`.
#' @return A data frame with two columns: `outcome` and `count`.
#'
#' @noRd
roll_outcome_count <- function(roll, env = parent.frame()) {

  # Check if roll is already a data frame
  if (tryCatch(is.data.frame(eval(roll, env)), error = function(e) FALSE)) {
    return(eval(roll, env))
  }

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
      n = yac("Multiply", paste0(l$n, collapse = ","))
    )
  }, dice_out)

  # Summarise table by outcome, adding counts
  df <- stats::aggregate(
    n ~ outcome,
    data = do.call(rbind, roll_out),
    function(l) yac("Add", paste0(l, collapse = ","))
  )

  # Calculate relative frequency
  t <- yac("Add", paste0(df$n, collapse = ","))
  df$d <- sapply(df$n, function(n) yac("", paste0(n, "/", t)))

  # Calculate cumulative sum (probability)
  df$p <- Reduce(
    function(x, y) yac("Add", paste0(x, ",", y)),
    df$d,
    accumulate = TRUE
  )

  # Fix d column
  names(df$d) <- NULL

  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(df))
  } else {
    return(df)
  }
}
