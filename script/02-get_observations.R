

##### In folder "small_test/obs_summary", there the simulation has been changed
##### to only allow smaller clades simulation (<10)
# Get simulation outputs with IW model using DAISIE simulation framework

 rm(list=ls())
# # Load the package,
 # !!!!!!! make sure it's on develop branch
devtools::load_all("/Users/yangshen/Downloads/phd_yang/pkgs/DAISIE")

iw_DAISIE_sim <- function(parameters) {
  success <- FALSE  # Initialize a flag to check if the simulation succeeded

  while (!success) {  # Keep trying until the simulation succeeds
    tryCatch({
      # Attempt to run the simulation
      iw_sim <- DAISIE_sim_cr(
        time = 5,
        M = 1000,
        pars = as.numeric(c(parameters[1], parameters[2], parameters[3], parameters[4], parameters[5])),
        replicates = 1,
        divdepmodel = "IW",
        nonoceanic_pars = c(0, 0),
        sample_freq  = Inf,
        plot_sims = FALSE,
        verbose = FALSE,
        cond = 1
      )

      # Calculate the number of clades
      num_clades <- length(iw_sim[[1]]) - 1

      # Check if the number of clades is greater than 10
      if (num_clades > 10) {
        message("Number of clades is larger than 10. Retrying the simulation...")
        success <- FALSE  # Retry the simulation
      } else {
        # If the number of clades is acceptable, set success to TRUE and exit the loop
        success <- TRUE
        message("Simulation completed successfully.")
      }

    }, error = function(e) {
      # If an error occurs, display a message and retry
      message("Error encountered: ", e$message)
      message("Retrying the simulation...")
      # The loop will continue automatically as success is still FALSE
    })
  }

  return(iw_sim)  # Return the successful simulation result
}


# # Read data
 parameter_space <- utils::read.csv("data/parameter_space_rep100_small_k.csv")
#
# # Initialize the space for outputs
 iw_sim_list <- list()

 random_seed <- sample(1:1e6, 1)
 set.seed(random_seed)
 cat("the seed is: ", random_seed)
 #291837

# # Iterate through each row of the parameter space
for (i in seq_len(nrow(parameter_space))) {
   cat("rep is ", i, "\n")
  # Extract the parameter set
  parameters <-parameter_space[i, c("lac", "mu", "K", "gam", "laa")]

  # Show the exact parameter
  cat("Running simulation for parameter set:\n")
  cat("lac=", parameters$lac, ", mu=", parameters$mu, "K=", parameters$K, "gam=",
      parameters$gam, "laa=", parameters$laa)

  # Run the simulation with the current parameter set
  iw_sim_list[[i]] <- iw_DAISIE_sim(parameters = parameters)

}
#
 iw_observations <- lapply(iw_sim_list, function(x) x[[1]])
#
 saveRDS(iw_observations, "data/iw_observations_rep100_small_k.rds")









