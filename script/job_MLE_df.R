# Run on cluster
library(iwABC)

# Maximum-likelihood estimation

# Read data

parameter_space <- utils::read.csv("~/iwABC/script/parameter_space.csv")
iw_observations <- readRDS("~/iwABC/script/iw_observations.rds")

# this data is from Shu's parameter setting but plus the initial K setting,
# and it's only 1 replication for each parameter combination.

# Initialise space
MLE_allpars <- list()

for (i in 1:nrow(parameter_space)) {
  # Extract simulation output
  the_sim <- iw_observations[[i]]

  # Extract the initial parameters for simulation
  pars_use <- parameter_space[i, ]

  # Record seed for each estimation
  seed_mle <-as.integer(Sys.time()) %% 1000000L * sample(1:10,1)
  set.seed(seed_mle)
  message("seed_mle: ", seed_mle)

  message("initial pars used:", paste(pars_use, collapse = " "))

  MLE_allpars[[i]] <- iwABC::get_MLE(the_sim = the_sim,
                                     pars_use = pars_use)

}

# Convert the list of results into a data frame
MLE_df <- do.call(rbind, lapply(MLE_allpars, as.data.frame))


saveRDS(MLE_df,
     file = "~/iwABC/script/MLE_df.rds")
