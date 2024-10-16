# Get simulation outputs with IW model using DAISIE simulation framework

rm(list=ls())
# Load the package
library(DAISIE)

iw_DAISIE_sim <- function(parameters) {

    iw_sim <- DAISIE::DAISIE_sim_cr(
      time = 5,
      M = 100,
      pars = as.numeric(c(parameters[1],parameters[2],parameters[3],parameters[4],parameters[5])),
      replicates = 1,
      divdepmodel = "IW",
      nonoceanic_pars = c(0, 0),
      sample_freq  = Inf,
      plot_sims = FALSE,
      verbose = FALSE,
      cond = 1 # Here I don't know why not empty for island
    )

  return(iw_sim)
}

# Read data
parameter_space <- read.csv("data/parameter_space.csv")

# Initialize the space for outputs
iw_sim_list <- list()

# Iterate through each row of the parameter space
for (i in seq_len(nrow(parameter_space))) {

  # Extract the parameter set
  parameters <-parameter_space[i, c("lac", "mu", "K", "gam", "laa")]

  # Show the exact parameter
  cat("Running simulation for parameter set:\n")
  cat("lac= ", parameters$lac, ", mu= ", parameters$mu, "K= ", parameters$K, "gam= ",
      parameters$gam, "laa= ", parameters$laa)

  # Run the simulation with the current parameter set
  iw_sim_list[[i]] <- iw_DAISIE_sim(parameters = parameters)

}

iw_observations <- lapply(iw_sim_list, function(x) x[[1]])

saveRDS(iw_observations, "data/iw_observations.rds")









