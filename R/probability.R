
# Calculate how many ways each possible outcome can be obtained
dice_outcome_count <- function(faces, n = 1) {

  # Polynomial expression for expansion (handled by Yacas)
  yac <- paste0("Expand((", paste0("x^", faces, collapse = "+"), ")^", n, ")")
  poly <- Ryacas::yac_str(yac)

  # Expanded polynomial expression and add trivial 1s)
  poly <- gsub("x(\\+|$)", "x^1\\1", gsub("(^|\\+)x", "\\11*x", poly))

  # Convert coeficients into outcome counts
  Map(
    function(l) list(outcome = as.numeric(l[2]), count = as.numeric(l[1])),
    strsplit(strsplit(poly, "\\+")[[1]], "\\*?x\\^")
  )
}

# Check if expression belongs to the Dice S4 class
is_die <- function(expr, env) {
  tryCatch(class(eval(expr, env)), error = function(e) "None") == "Dice"
}

# Get dice_outcome_count() of an expression
get_doc <- function(expr, env, dice = FALSE) {

  # Evaluate and get DOC (if expression is of the form N * dS)
  if (dice) {
    expr <- bquote(dice_outcome_count(faces(.(expr[[3]])), .(expr[[2]])))
    return(eval(expr, env))
  }

  # Return DOC directly (if expression is a single die)
  return(dice_outcome_count(faces(eval(expr, env))))
}

# Mask dice as inputs of a function (and get their dice_outcome_count())
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

# Mask dice of a roll as inputs of a function (and their dice_outcome_count())
mask_roll <- function(expr_and_counts, env) {

  # Separate parts of input
  expr <- expr_and_counts[[1]]
  counts <- expr_and_counts[[2]]

  # Mask dice from LHS of expression
  if (is_die(expr[[2]], env)) {
    out <- mask_dice(list(expr[[2]], counts), env)
    expr[[2]] <- out[[1]]; counts <- out[[2]]
  }

  # Handle parentheses and unary functions
  if (length(expr) == 2) {
    out <- mask_roll(list(expr[[2]], counts), env)
    expr[[2]] <- out[[1]]; counts <- out[[2]]

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
    expr[[3]] <- out[[1]]; counts <- out[[2]]
  }

  # Iterate over both branches of the expression
  for (i in idx) {
    out <- mask_roll(list(expr[[i]], counts), env)
    expr[[i]] <- out[[1]]; counts <- out[[2]]
  }

  return(list(expr, counts))
}

#' Calculate the absolute frequency of each outcome of a roll formula
#' @param roll Roll formula
#' @export
roll_outcome_count <- function(roll) {

  # Capture roll expression and mask dice objects
  expr_and_counts <- mask_roll(list(substitute(roll), list()), parent.frame())

  # Create a function that inputs values in the roll formula (final outcome)
  roll_function <- function(...) {
    dots <- list(...)
    eval(expr_and_counts[[1]], envir = list(dots))
  }

  # Transpose list of dice outcomes
  dice_out <- Map(
    function(l) do.call(Map, c(f = list, l)),
    cross(expr_and_counts[[2]])
  )

  # Actually roll dice with roll_function() and multiply outcomes' counts
  roll_out <- Map(function(l) data.frame(
    outcome = do.call(roll_function, as.list(l$outcome)),
    count = do.call(prod, l$count)
  ), dice_out)

  # Summarise table by outcome, adding counts
  df <- stats::aggregate(count ~ outcome, data = do.call(rbind, roll_out), sum)

  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(df))
  } else {
    return(df)
  }
}
