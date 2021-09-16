# plot coefficients
plot_coef <- function(coef){
  coef_p <- tidyr::pivot_longer(coef, cols = c("estimate", "lower", "upper")) 
  coef_p$term %<>% as.character()
  ggplot(data = coef_p, aes(x = value, y = term, group = term)) + 
    geom_point() +
    geom_line()
}