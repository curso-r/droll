
# Is this call a d()?
is_lang_roll <- function(lang) {
  lang[[1]] == "d"
}

# Is this call a leaf of the AST?
is_lang_leaf <- function(lang) {
  all(purrr::map_lgl(lang[2:3], rlang::is_syntactic_literal))
}

# Evaluate one step of the call (and split possible dice values)
lang_eval_one <- function(lang) {

  # Evaluate leaf nodes and split dices into 1:Ns
  if (is_lang_roll(lang)) {
    return(1:lang[[2]])
  } else if (is_lang_leaf(lang)) {
    return(eval(lang))
  }

  # DFS to evaluate only one step of the call
  i <- which(purrr::map_chr(lang, typeof) == "language")[1]
  purrr::map(lang_eval_one(lang[[i]]), ~{lang[[i]] <- .x; lang})
}

# Evaluate all steps of the call (and count equivalent outcomes)
lang_eval_all <- function(lang_df) {

  # Stop evaluating if call is a number
  if (is.numeric(lang_df$lang[1])) {
    return(lang_df)
  }

  # Count equivalent outcomes, evaluate a step and repeat
  lang_df %>%
    dplyr::group_by(lang) %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::mutate(lang = purrr::map(lang, lang_eval_one)) %>%
    tidyr::unnest(lang) %>%
    lang_eval_all()
}

# Calculate the probability of each outcome of a dice formula
dice_prob <- function(dice) {

  dice %>%
    dice_prob_preprocess() %>%
    base::list() %>%
    dplyr::tibble(lang = ., n = 1) %>%
    lang_eval_all() %>%
    dplyr::rename(result = lang) %>%
    dplyr::arrange(result)
}
