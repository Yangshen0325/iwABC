
rm(list = ls())
library(tidyverse)
##### Preview some results

res <- readRDS("/Users/yangshen/Downloads/phd_yang/ABC_IW/param_set_1_ss_0.rds")

# true parameters
parameter_space <- read.csv("data/parameter_space_rep100_large_k.csv")

unique_parameter_space <- parameter_space |>
  select(lac, mu, K, gam, laa) |>
  distinct()

# For this example, the true parameters are the first row of the parameter space
true_parameters <- unique_parameter_space[2, ]

print(true_parameters)

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
  #scale_y_log10() +
  facet_wrap(~parameter, scales = "free") +
  geom_hline(data = tibble(parameter = names(true_parameters), value = unlist(true_parameters)),
             aes(yintercept = value),
             linetype = "dashed", color = "red", linewidth = 0.6)
