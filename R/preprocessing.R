
dice_regex <- "([:digit:]*?)[dD]([:digit:]+)"

roll_parse_expr <- function(expr) {
  expr %>%
    stringr::str_remove_all("\\s") %>%
    stringr::str_replace_all(dice_regex, "d(\\2,\\1)") %>%
    stringr::str_remove_all(",(?=\\))") %>%
    rlang::parse_expr()
}
