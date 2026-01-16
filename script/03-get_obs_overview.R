
# Get the overview of observation datasets,
# e.g. mean of number of clades (sd), mean of total species richness (sd),
# mean of largest clades (sd), mean of first clade size (sd)

# 2105_25 Have a look at the overview of 1000 for each parameter combinations datasets
# (not the data for MLE, because Hanno regenerate the data sets on his side)

rm(list=ls())
library(tidyverse)
library(dplyr)

# Read the observations
iw_observations <- readRDS("data/iw_observations.rds") # each combination has 1000 reps, 48000 in total

# Total number of species and clades
# Initialize a data frame to store the summary
# summary_obs <- data.frame(total_sp = numeric(length(iw_observations)),
#                           end_sp = numeric(length(iw_observations)),
#                           nonend_sp = numeric(length(iw_observations)),
#                           no_clades = numeric(length(iw_observations)),
#                           largest_clade_size = numeric(length(iw_observations)),
#                           first_clade_size = numeric(length(iw_observations)))
#
# for (i in seq_along(iw_observations)) {
#   # Extract the stt_all matrix
#   stt_all <- iw_observations[[i]][[1]][["stt_all"]]
#
#   summary_obs$total_sp[i] <- sum(stt_all[nrow(stt_all), c("nI", "nA", "nC")])
#   summary_obs$end_sp[i] <- sum(stt_all[nrow(stt_all), c("nA", "nC")])
#   summary_obs$nonend_sp[i] <- sum(stt_all[nrow(stt_all), c("nI")])
#   summary_obs$no_clades[i] <- length(iw_observations[[i]]) - 1
#   summary_obs$largest_clade_size[i] <- largest_clade_size(iw_observations[[i]])
#   summary_obs$first_clade_size[i] <- first_clade_size(iw_observations[[i]])
#
# }
#saveRDS(summary_obs, file = "script/summary_obs.rds")
reps <- 1000
get_each_rep_res <- function(each_rep_res){

  summary_obs <- data.frame(total_sp = numeric(reps),
                            end_sp = numeric(reps),
                            nonend_sp = numeric(reps),
                            no_clades = numeric(reps),
                            largest_clade_size = numeric(reps),
                            first_clade_size = numeric(reps))
  for(i in 1:reps) {
    # Extract the stt_all matrix
    stt_all <- each_rep_res[[i]][[1]][["stt_all"]]

    summary_obs$total_sp[i] <- sum(stt_all[nrow(stt_all), c("nI", "nA", "nC")])
    summary_obs$end_sp[i] <- sum(stt_all[nrow(stt_all), c("nA", "nC")])
    summary_obs$nonend_sp[i] <- sum(stt_all[nrow(stt_all), c("nI")])
    summary_obs$no_clades[i] <- length(each_rep_res[[i]]) - 1
    summary_obs$largest_clade_size[i] <- largest_clade_size(each_rep_res[[i]])
    summary_obs$first_clade_size[i] <- first_clade_size(each_rep_res[[i]])
  }
  return(summary_obs)
}

obs_overview_1000 <- lapply(iw_observations, get_each_rep_res)
obs_overview_1000 <- bind_rows(obs_overview_1000, .id = "column_label")

saveRDS(obs_overview_1000, file = "script/obs_overview_1000.rds")

# Visualize ---------------------------------------------------------------

library(tidyr)
library(ggplot2)

# Read the parameter space
param_data <- read_csv("data/parameter_space.csv")
#summary_obs <- readRDS("script/summary_obs.rds")
#obs_combined <- bind_cols(param_data %>% select(-reps), summary_obs)


# Read the summary data
obs_overview_1000 <- readRDS("script/obs_overview_1000.rds")
obs_overview_1000$column_label <- factor(obs_overview_1000$column_label,
                                        levels = as.character(1:48))
# Pivot to long format for ggplot
obs_overview_1000_long <- obs_overview_1000 %>%
  pivot_longer(-column_label, names_to = "metric", values_to = "value")

# Boxplot
ggplot(obs_overview_1000_long, aes(x = factor(column_label), y = value)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = "free_y") +
  theme_bw() +
  labs(x = "Scenario", y = "Value") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggsave("script/obs_overview_1000_boxplot.png", width = 10, height = 6)


summary_obs |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value") |>
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free")


