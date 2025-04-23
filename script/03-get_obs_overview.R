
# Get the overview of observation datasets,
# e.g. mean of number of clades (sd), mean of total species richness (sd),
# mean of largest clades (sd), mean of first clade size (sd)


rm(list=ls())
library(tidyverse)

# Read the observations
iw_observations <- readRDS("data/iw_observations.rds") # each combination has 10 reps, 480 in total

# Total number of species and clades
# Initialize a data frame to store the summary
summary_obs <- data.frame(total_sp = numeric(length(iw_observations)),
                          end_sp = numeric(length(iw_observations)),
                          nonend_sp = numeric(length(iw_observations)),
                          no_clades = numeric(length(iw_observations)),
                          largest_clade_size = numeric(length(iw_observations)),
                          first_clade_size = numeric(length(iw_observations)))

for (i in seq_along(iw_observations)) {
  # Extract the stt_all matrix
  stt_all <- iw_observations[[i]][[1]][["stt_all"]]

  summary_obs$total_sp[i] <- sum(stt_all[nrow(stt_all), c("nI", "nA", "nC")])
  summary_obs$end_sp[i] <- sum(stt_all[nrow(stt_all), c("nA", "nC")])
  summary_obs$nonend_sp[i] <- sum(stt_all[nrow(stt_all), c("nI")])
  summary_obs$no_clades[i] <- length(iw_observations[[i]]) - 1
  summary_obs$largest_clade_size[i] <- largest_clade_size(iw_observations[[i]])
  summary_obs$first_clade_size[i] <- first_clade_size(iw_observations[[i]])

}
#saveRDS(summary_obs, file = "script/summary_obs.rds")


# Visualize ---------------------------------------------------------------

library(tidyr)
library(ggplot2)

# Read the parameter space
param_data <- read_csv("data/parameter_space.csv")
#summary_obs <- readRDS("script/summary_obs.rds")

obs_combined <- bind_cols(param_data %>% select(-rep), summary_obs)

# obs_summary <- obs_combined %>%
#   group_by(lac, mu, K, gam, laa) %>%
#   summarise(across(
#     everything(),
#     list(mean = mean, sd = sd),
#     .names = "{.col}_{.fn}"
#   ), .groups = "drop")

# library(knitr)
# library(kableExtra)
#
# # Choose columns to include (example)
# obs_summary %>%
#   select(lac, mu, K, gam, laa,
#          total_sp_mean, total_sp_sd,
#          end_sp_mean, end_sp_sd) %>%
#   kable(format = "latex", booktabs = TRUE, digits = 2,
#         caption = "Summary statistics per parameter combination") %>%
#   kable_styling(latex_options = "striped")

# Add a scenario label
obs_combined <- obs_combined %>%
  mutate(scenario = rep(1:48, each = 10))

# Pivot to long format for ggplot
obs_long <- obs_combined %>%
  select(scenario, total_sp:first_clade_size) %>%
  pivot_longer(-scenario, names_to = "metric", values_to = "value")

# Boxplot
ggplot(obs_long, aes(x = factor(scenario), y = value)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = "free_y") +
  theme_bw() +
  labs(x = "Scenario", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))



summary_obs |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value") |>
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free")


