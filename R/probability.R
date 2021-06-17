
# Calculate how many ways each possible outcome can be obtained
dice_outcome_count <- function(sides, n = 1) {

  # Polynomial expression for expansion (handled by Yacas)
  yac <- paste0("Expand((", paste0("x^", 1:sides, collapse = "+"), ")^", n, ")")
  poly <- Ryacas::yac_str(yac)

  # Expanded polynomial expression and add trivial 1s)
  poly <- gsub("x(\\+|$)", "x^1\\1", gsub("(^|\\+)x", "\\11*x", poly))

  # Convert coeficients into outcome counts
  Map(
    function(l) list(outcome = as.numeric(l[2]), count = as.numeric(l[1])),
    strsplit(strsplit(poly, "\\+")[[1]], "\\*?x\\^")
  )
}

#' Calculate the absolute frequency of each outcome of a roll formula
#' @param roll Roll formula
#' @export
roll_outcome_count <- function(roll) {

  # Clean roll formula
  roll <- gsub("\\s", "", roll)

  # Extract dice from roll formula
  roll_dice <- regmatches(roll, gregexpr(dice_regex, roll))[[1]]

  # Create the body of roll_function(), taking dots as input
  roll_body <- Reduce(
    f = function(s, i) sub(dice_regex, paste0("dots[[", i, "]]"), s),
    x = seq_along(roll_dice), init = roll
  )

  # Create a function that inputs values in the roll formula (final outcome)
  roll_function <- eval(parse(
    text = paste("function(...) { dots <- list(...);", roll_body, "}")
  ))

  # Extract arguments for dice_outcome_count()
  dice_args <- Map(
    f = function(d) as.list(rev(as.numeric(d))),
    regmatches(roll_dice, gregexpr("[0-9]+", roll_dice))
  )

  # Calculate dice outcomes and get all permutations
  dice_perm <- cross(Map(function(a) do.call(dice_outcome_count, a), dice_args))

  # Transpose list of dice outcomes
  dice_out <- Map(function(l) do.call(Map, c(f = c, l)), dice_perm)

  # Actually roll dice with roll_function() and multiply outcomes' counts
  roll_out <- Map(function(l) data.frame(
    outcome = do.call(roll_function, as.list(l$outcome)),
    count = prod(l$count)
  ), dice_out)

  # Summarise table by outcome, adding counts
  stats::aggregate(count ~ outcome, data = do.call(rbind, roll_out), sum)
}
