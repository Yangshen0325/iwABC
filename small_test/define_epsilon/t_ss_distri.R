
# It takes time, so on cluster



# Check out the distribution of differences of summary statistics, then define epsilon

rm(list = ls())
library(iwABC)
# function to get ss_diff -----------------------------------------------

# return `ss_diff` for only one particle (a set of parameters), but run 500 times to get 500 differences

get_ss_diff <- function(
    obs_data,
    calc_ss_function = calc_ss_iw,
    prior_generating_function = prior_gen,
    prior_density_function = prior_dens,
    number_of_particles = 500,
    idparsopt = c(1, 2, 3, 4, 5),
    pars,
    ss_set = 0
) {

  # Intialize the output
  ss_diff <- c()

  for(rep in 1:number_of_particles) {

    # Resample parameters until they meet the prior density condition
    parameters <- prior_generating_function(pars = pars, idparsopt = idparsopt)
    while (prior_density_function(parameters, idparsopt) <= 0) {
      parameters <- prior_generating_function(pars = pars, idparsopt = idparsopt)
    }

      # Simulate a new tree, given the proposed parameters
      new_sim <- DAISIE::DAISIE_sim_cr(
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

      # Calculate the summary statistics for the simulated tree
      df_stats <- calc_ss_function(sim1 = obs_data,
                                   sim2 = new_sim[[1]],
                                   ss_set = ss_set)

      # Add the difference to the output
      ss_diff <- rbind(ss_diff, df_stats)
  }
  return(ss_diff)
}





# Distribution of difference across different combinations of parameters --------

# read empirical data, 2^5 combinations of parameters, each with 10 reps, so 320 lists
iw_observations <- readRDS("iw_observations.rds")

# read parameters generating empirical data, correponding 320 observations
parameter_space <- read.csv("parameter_space.csv")

# initialize the results
ss_diff_list <- list()

# Get the returns across all scenarios
for (i in seq_along(iw_observations)){

  obs_sim <- iw_observations[[i]]
  the_par <- as.numeric(parameter_space[i, c(1:5)])

  # calculate the differences of summary statistics
  ss_diff_list[[i]] <- get_ss_diff(obs_data = obs_sim,
                                           calc_ss_function = calc_ss_iw,
                                           prior_generating_function = prior_gen,
                                           prior_density_function = prior_dens,
                                           number_of_particles = 500,
                                           idparsopt = c(1, 2, 3, 4, 5),
                                           pars = the_par,
                                           ss_set = 0)

}

saveRDS(ss_diff_list, file = "~/iwABC/small_test/define_epsilon/t_ss_distri.rds")




