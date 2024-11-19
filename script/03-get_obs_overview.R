



# Read initial parameters used in simulation for observed data
parameter_space <- read.csv("script/parameter_space.csv")

# Total number of species and clades
# Initialize a data frame to store the summary
summary_obs <- data.frame(total_sp = numeric(length(iw_observations)),
                          no_clades = numeric(length(iw_observations)))

for (i in seq_along(iw_observations)) {
  # Extract the stt_all matrix
  stt_all <- iw_observations[[i]][[1]][["stt_all"]]

  summary_obs$total_sp[i] <- sum(stt_all[nrow(stt_all), c("nI", "nA", "nC")])
  summary_obs$no_clades[i] <- length(iw_observations[[i]]) - 1

}



