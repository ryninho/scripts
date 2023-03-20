plot_coef <- function(lm_object, name, ylim = c(-0.05, 0.05)) {
  lm_object %>% 
    tidy(.) %>% 
    mutate(
      coef_low = estimate - 1.96 * std.error,
      coef_high = estimate + 1.96 * std.error,
      term = factor(term, levels = term)
      ) %>%
    filter(term != "(Intercept)") %>%
    ggplot(aes(x = term, y = estimate)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    geom_errorbar(aes(ymin = coef_low, ymax = coef_high), alpha = 0.25) +
    coord_flip(ylim = ylim) +
    ggtitle(name)
}
