

##### In folder "small_test/obs_summary", there the simulation has been changed
##### to only allow smaller clades simulation (<10).  1405_25 Not the case anymore.
##### Generate 1000 simulations, and run the estimation stop at 100 successful jobs.



# Get simulation outputs with IW model using DAISIE simulation framework

 rm(list=ls())
# # Load the package,
 # !!!!!!! make sure it's on develop branch
devtools::load_all("/Users/yangshen/Downloads/phd_yang/pkgs/DAISIE")
library(purr)

iw_DAISIE_sim <- function(parameters, seed) {

      set.seed(seed)

      # Run the simulation
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

  return(iw_sim)  # Return the successful simulation result

}

# # Read data
 parameter_space <- utils::read.csv("data/parameter_space.csv")
#
# # Initialize the space for outputs
 iw_sim_list <- list()
 get_reps <- 1000 # can change this if needed

 # random_seed <- sample(1:1e6, 1)
 # set.seed(random_seed)
 # cat("the seed is: ", random_seed)
 #291837

# # Iterate through each row of the parameter space
for (i in seq_len(nrow(parameter_space))) {

  cat("rep is ", i, "\n")
   # Extract the parameter set
   parameters <-parameter_space[i, c("lac", "mu", "K", "gam", "laa")]

   # Show the exact parameter
   cat("Running simulation for parameter set:\n")
   cat("lac=", parameters$lac, ", mu=", parameters$mu, "K=", parameters$K, "gam=",
       parameters$gam, "laa=", parameters$laa, "\n")

   sim_results <- map(1:get_reps, function(j) {

     seed <- sample.int(1e6, 1)
     res <- iw_DAISIE_sim(parameters = parameters,
                          seed = seed)
     list(
       param_id = i,
       rep_id = j,
       seed = seed,
       output = res
     )
   })
   # Run the simulation with the current parameter set
   iw_sim_list[[i]] <- sim_results

}

 # Save the results in "script folder"
 saveRDS(iw_sim_list, "script/iw_sim_list.rds")

 # Save the observations to data
 iw_observations <- map(iw_sim_list, ~ map(.x, ~ .x$output[[1]]))
 saveRDS(iw_observations, "data/iw_observations.rds")

 ## If I want to know the seed used for each simulation
 # seed_list <- map(iw_sim_list, ~ map(.x, ~ .x$seed))
 # seed_df <- map2_dfr(
 #   .x = seed_list,
 #   .y = seq_along(seed_list),
 #   .f = ~ tibble(
 #     param_id = .y,
 #     rep_id = seq_along(.x),
 #     seed = unlist(.x)
 #   )
 # )







