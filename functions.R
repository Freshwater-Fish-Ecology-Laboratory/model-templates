# plot coefficients
plot_coef <- function(coef){
  coef_p <- tidyr::pivot_longer(coef, cols = c("estimate", "lower", "upper")) 
  coef_p$term %<>% as.character()
  ggplot(data = coef_p, aes(x = value, y = term, group = term)) + 
    geom_point() +
    geom_line()
}

library(purrr)
library(dplyr)
### convert mcmc sample to df
samples_to_df <- function(samples){
  if(is.null(names(samples)))
    samples <- list(chain1 = samples)
  sample_df <- map_df(seq_along(samples), function(x){
    df <- as.data.frame(samples[[x]])
    df %>%
      tidyr::gather(key="parameter", value="value") %>%
      group_by(parameter) %>%
      mutate(index = 1:n()) %>%
      ungroup() %>%
      mutate(chain = factor(x))
  })
 sample_df
}

### plot mcmc sample trace
plot_sample_trace <- function(samples){
  df <- samples_to_df(samples)
  ggplot(data = df, aes(y = value, x = index, 
                             color = chain)) +
    geom_line(size = 0.2) +
    facet_wrap(~parameter, scales = "free_y")
}

### plot mcmc sample posterior density
plot_sample_density <- function(samples){
  df <- samples_to_df(samples)
  ggplot(data = df, aes(x = value, 
                             color = chain,
                             fill = chain)) +
    geom_density(alpha = 0.3) +
    facet_wrap(~parameter, scales = "free")
}

### plot parameter estimates
summary_to_df <- function(summary){
  df <- as_tibble(summary)
  df$Parameter <- rownames(summary)
  df %>%
    select(Parameter, everything()) %>%
    rename(Lower95 = `95%CI_low`,
           Upper95 = `95%CI_upp`,
           StDev = `St.Dev.`)
}

plot_estimates <- function(summary, median = TRUE){
  summary_df <- summary_to_df(summary)
  gp <- ggplot(summary_df, aes(y = Mean, x = Parameter))
  if(median)
    gp <- ggplot(summary_df, aes(y = Median, x = Parameter))
   gp +
    geom_point() +
    geom_errorbar(aes(ymin = Lower95, ymax = Upper95),
                  colour="black", width=.3)
}


