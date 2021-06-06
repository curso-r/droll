
dice_regex <- "([:digit:]*?)[dD]([:digit:]+)"

dice_prob_preprocess <- function(dice) {

  dice %>%
    stringr::str_remove_all("\\s") %>%
    stringr::str_replace_all(dice_regex, "({paste0(rep('d(\\2)',\\1),collapse = '+')})") %>%
    stringr::str_remove_all(",(?=\\))") %>%
    stringr::str_replace_all("([*+/\\-\\^])", " \\1 ") %>%
    stringr::str_glue() %>%
    rlang::parse_expr()
}
