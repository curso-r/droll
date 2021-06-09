
# Calculate how many ways each possible outcome of dice can be obtained
dice_outcome_count <- function(sides, n = 1) {

  # Sigma term of https://mathworld.wolfram.com/Dice.html
  sigma <- function(p) {
    0:floor((p - n)/sides) %>%
      purrr::map(~(-1)^.x * gmp::chooseZ(n, .x) * gmp::chooseZ(p-sides*.x-1, n-1)) %>%
      purrr::reduce(`+`) %>%
      gmp::asNumeric()
  }

  # Iterate over all possible values dice can add up to
  n:(sides*n) %>%
    purrr::map(~list(outcome = .x, count = purrr::map_dbl(.x, sigma)))
}

# Calculate the absolute frequency of each outcome of a dice formula
roll_outcome_count <- function(roll) {

  # Clean roll expression
  roll <- stringr::str_remove_all(roll, "\\s")

  # Create a function that takes dice's values and returns the roll's outcome
  roll_function <- roll %>%
    stringr::str_count(dice_regex) %>%
    base::seq(1, .) %>%
    purrr::reduce(~stringr::str_replace(.x, dice_regex, paste0("dots[[", .y, "]]")), .init = roll) %>%
    stringr::str_c("function(...) { dots <- list(...); ", ., " }") %>%
    rlang::parse_expr() %>%
    rlang::eval_tidy()

  # Combine the outcomes of each die and calculate the full outcome
  roll %>%
    stringr::str_extract_all(dice_regex, simplify = TRUE) %>%
    purrr::map(stringr::str_extract_all, "[:digit:]+", simplify = TRUE) %>%
    purrr::map(~rev(as.numeric(.x))) %>%
    purrr::map(~purrr::exec(dice_outcome_count, !!!.x)) %>%
    purrr::cross() %>%
    purrr::map(purrr::transpose) %>%
    purrr::map(~list(
      outcome = purrr::lift_dl(roll_function)(.x$outcome),
      count = prod(as.numeric(.x$count))
    )) %>%
    purrr::transpose() %>%
    purrr::map(as.numeric) %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(count = sum(count)) %>%
    dplyr::arrange(outcome)
}
