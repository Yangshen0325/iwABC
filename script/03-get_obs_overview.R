
rm(list=ls())
# Read the observations
iw_observations <- readRDS("script/iw_observations.rds")


# Total number of species and clades
# Initialize a data frame to store the summary
summary_obs <- data.frame(total_sp = numeric(length(iw_observations)),
                          end_sp = numeric(length(iw_observations)),
                          nonend_sp = numeric(length(iw_observations)),
                          no_clades = numeric(length(iw_observations)))

for (i in seq_along(iw_observations)) {
  # Extract the stt_all matrix
  stt_all <- iw_observations[[i]][[1]][["stt_all"]]

  summary_obs$total_sp[i] <- sum(stt_all[nrow(stt_all), c("nI", "nA", "nC")])
  summary_obs$end_sp[i] <- sum(stt_all[nrow(stt_all), c("nA", "nC")])
  summary_obs$nonend_sp[i] <- sum(stt_all[nrow(stt_all), c("nI")])
  summary_obs$no_clades[i] <- length(iw_observations[[i]]) - 1

}
saveRDS(summary_obs, file = "script/summary_obs.rds")


# Viasulise ---------------------------------------------------------------

library(tidyverse)

summary_obs |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value") |>
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free")


