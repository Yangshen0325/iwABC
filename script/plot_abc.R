
## To check the results of the ABC analysis.


want_to_plot <- FALSE

if (want_to_plot) {

  require(tidyverse)

  res <- readRDS("/Users/thijsjanzen/Documents/GitHub/iwABC/results/param_set_1_ss_0.rds")

  to_plot <- c()
  for (i in 1:length(res$ABC)) {
    to_add <- cbind(i, res$ABC[[i]])
    to_plot <- rbind(to_plot, to_add)
  }

  colnames(to_plot) <- c("iteration", "lac", "mu", "K", "gam", "laa")
  to_plot <- as_tibble(to_plot)
  to_plot %>%
    gather(key = parameter, value = "value", -iteration) %>%
    ggplot(aes(x = iteration, y = value, group = iteration)) +
    geom_boxplot() +
    scale_y_log10() +
    facet_wrap(~parameter, scales = "free")

}

