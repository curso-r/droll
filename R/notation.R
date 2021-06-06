#
#
# expression_parse <- function(expr) {
#
#   expr %>%
#     stringr::str_remove_all("\\s") %>%
#     stringr::str_replace_all(dice_regex, "d(\\2,\\1)") %>%
#     stringr::str_remove_all(",(?=\\))") %>%
#     stringr::str_replace_all("([*+/\\-\\^])", " \\1 ")
#
# }
#
# expression_eval <- function(expr) {
#
#   eval(parse(text = expr))
#
# }
#
# expression_probabilities <- function(expr) {
#
#   calculate_probabilities <- function(dice, curr) {
#
#     if (length(dice) == 0L) return(curr)
#
#     dice %>%
#       purrr::pluck(1) %>%
#       purrr::map_dfr(~dplyr::mutate(curr, total = total + .x)) %>%
#       dplyr::group_by(total) %>%
#       dplyr::summarise(n = sum(n)) %>%
#       calculate_probabilities(dice[-1], .)
#
#   }
#
#   expr %>%
#     stringr::str_remove_all("\\s") %>%
#     stringr::str_extract_all(dice_regex) %>%
#     purrr::pluck(1) %>%
#     stringr::str_replace(dice_regex, "rep(\\2,\\1)") %>%
#     purrr::map(~eval(parse(text = .x))) %>%
#     purrr::flatten_dbl() %>%
#     stringr::str_c("1:", .) %>%
#     purrr::map(~eval(parse(text = .x))) %>%
#     calculate_probabilities(dplyr::tibble(total = 0L, n = 1L)) %>%
#     dplyr::arrange(total)
#
# }
#
# dice_roll <- function(dice) {
#
#   dice %>%
#     expression_parse() %>%
#     expression_eval()
#
# }
#
# dice_max <- function(dice) {
#
#   dice_clean <- stringr::str_remove_all(dice, "\\s")
#
#   while (stringr::str_detect(dice_clean, dice_regex)) {
#
#     dice_clean <- dice_clean %>%
#       stringr::str_extract(dice_regex) %>%
#       expression_probabilities() %>%
#       dplyr::slice_max(total) %>%
#       dplyr::pull(total) %>%
#       base::as.character() %>%
#       stringr::str_replace(dice_clean, dice_regex, .)
#
#   }
#
#   expression_eval(dice_clean)
# }
#
# dice_min <- function(dice) {
#
#   dice_clean <- stringr::str_remove_all(dice, "\\s")
#
#   while (stringr::str_detect(dice_clean, dice_regex)) {
#
#     dice_clean <- dice_clean %>%
#       stringr::str_extract(dice_regex) %>%
#       expression_probabilities() %>%
#       dplyr::slice_min(total) %>%
#       dplyr::pull(total) %>%
#       base::as.character() %>%
#       stringr::str_replace(dice_clean, dice_regex, .)
#
#   }
#
#   expression_eval(dice_clean)
# }
#
# dice_mean <- function(dice) {
#
#   dice %>%
#     expression_probabilities() %>%
#     dplyr::mutate(f = n*total) %>%
#     dplyr::summarise(f = sum(n*total)/sum(n)) %>%
#     dplyr::pull(f)
#
# }
#
# dice_sd <- function(dice) {
#
#   dice %>%
#     expression_probabilities() %>%
#     dplyr::mutate(f = n*total) %>%
#     dplyr::summarise(f = sd(total)) %>% # ?????
#     dplyr::pull(f)
#
# }
#
# plot_roll <- function(dice) {
#
#   df <- dice %>%
#     expression_probabilities() %>%
#     dplyr::mutate(f = round(n*100/sum(n), 1))
#
#   ggplot2::qplot(total, f, data = df, geom = "col")
# }
#
# plot_gte <- function(dice) {
#
#   df <- dice %>%
#     expression_probabilities() %>%
#     dplyr::arrange(-total) %>%
#     dplyr::mutate(n = cumsum(n)) %>%
#     dplyr::mutate(f = round(n*100/max(n), 1))
#
#   ggplot2::qplot(total, f, data = df, geom = "col")
# }
#
# plot_lte <- function(dice) {
#
#   df <- dice %>%
#     expression_probabilities() %>%
#     dplyr::mutate(n = cumsum(n)) %>%
#     dplyr::mutate(f = round(n*100/max(n), 1))
#
#   ggplot2::qplot(total, f, data = df, geom = "col")
# }
#
# prob_roll <- function(dice, value) {
#
#   dice %>%
#     expression_probabilities() %>%
#     dplyr::mutate(f = n/sum(n)) %>%
#     dplyr::filter(total == value) %>%
#     dplyr::pull(f)
#
# }
#
# prob_gte <- function(dice, value) {
#
#   dice %>%
#     expression_probabilities() %>%
#     dplyr::mutate(f = n/sum(n)) %>%
#     dplyr::filter(total >= value) %>%
#     dplyr::summarise(f = sum(f)) %>%
#     dplyr::pull(f)
#
# }
#
# prob_lte <- function(dice, value) {
#
#   dice %>%
#     expression_probabilities() %>%
#     dplyr::mutate(f = n/sum(n)) %>%
#     dplyr::filter(total <= value) %>%
#     dplyr::summarise(f = sum(f)) %>%
#     dplyr::pull(f)
#
# }
#
# prob_gt <- function(dice, value) {
#
#   dice %>%
#     expression_probabilities() %>%
#     dplyr::mutate(f = n/sum(n)) %>%
#     dplyr::filter(total > value) %>%
#     dplyr::summarise(f = sum(f)) %>%
#     dplyr::pull(f)
#
# }
#
# prob_lt <- function(dice, value) {
#
#   dice %>%
#     expression_probabilities() %>%
#     dplyr::mutate(f = n/sum(n)) %>%
#     dplyr::filter(total < value) %>%
#     dplyr::summarise(f = sum(f)) %>%
#     dplyr::pull(f)
#
# }
#
#
#
# # expr <- dice <- "3*2d6 + d20 + 1d10 - 1 +    4"
# #
# # dice_roll(dice)
# # dice_max(dice)
# # dice_min(dice)
# # dice_mean(dice)
# # dice_sd(dice)
# #
# # plot_roll(dice) # Está errado!
# # plot_gte(dice)
# # plot_lte(dice)
# #
# # prob_roll(dice, 15) # Está errado!
# # prob_gte(dice, 15)
# # prob_lte(dice, 15)
# # prob_gt(dice, 15)
# # prob_lt(dice, 15)
#
