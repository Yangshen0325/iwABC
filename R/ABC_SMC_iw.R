#' ABC approach to estimate parameters in IW.
#'
#' @param obs_data A list of simulation output as observation.
#' @param calc_ss_function A function to calculate summary statistic distance
#'  between simulated and observed data.
#' @param init_epsilon_values A vector of initial epsilon values.
#' @param prior_generating_function Function to generate parameters from the
#'  prior distribution.
#' @param prior_density_function Function to calculate the prior probability.
#' @param number_of_particles The number of particles in each iteration.
#' @param sigma Standard deviation of the perturbation distribution.
#' @param stop_rate A numeric value which is the boundary to stop the algorithm.
#' @param num_iterations The maximum number of iterations.
#' @param idparsopt The id of the parameters that need to be inferred, the others
#'  are fixed.
#' @param ss_set A numeric indicates which set of summary statistics that
#'  are used to calculate the distance.


ABC_SMC_iw <- function(
    obs_data,
    calc_ss_function,
    init_epsilon_values,
    prior_generating_function,
    prior_density_function,
    number_of_particles,
    sigma,
    stop_rate,
    num_iterations,
    idparsopt,
    pars
    #ss_set
) {

  # Generate a matrix with epsilon values
  epsilon <- matrix(nrow = 20, ncol = length(init_epsilon_values))
  epsilon[1, ] <- init_epsilon_values

  # Store weights
  new_weights <- c()
  new_params <- list(c(seq_along(pars)))
  previous_weights <- c()
  previous_params  <- list(c(seq_along(pars)))

  indices <- 1:number_of_particles

  n_iter <- 0

  # Initialize the output lists
  ABC_list <- list()
  sim_list <- list()
  ss_diff_list <- list()

  # Iterate through the number of iterations
  for (i in 1:num_iterations) {

    ss_diff <- c()

    n_iter <- n_iter + 1

    cat("\nGenerating Particles for iteration\t", i, "\n")
    cat("0--------25--------50--------75--------100\n")
    cat("*")
    utils::flush.console()
    print_frequency <- 20

    tried <- 0

    number_accepted <- 0

    sigma_temp <- sigma * exp(-0.2 * (i - 1))

    # Replace all vectors
    if (i > 1) {
      #normalize the weights and store them as previous weights.
      previous_weights <- new_weights / sum(new_weights)
      new_weights <- c() #remove all currently stored weights
      previous_params <- new_params #store found params
      new_params <- list(c(seq_along(parameters))) #clear new params
    }

    stoprate_reached <- FALSE

    while (number_accepted < number_of_particles) {

      # In this initial step, generate parameters from the prior
      if (i == 1) {
        parameters <- prior_generating_function(pars, idparsopt)
      } else {
        #if not in the initial step, generate parameters
        #from the weighted previous distribution:
        index <- sample(x = indices, size = 1,
                        replace = TRUE, prob = previous_weights)

        for (p_index in seq_along(parameters)) {
          parameters[p_index] <- previous_params[[index]][p_index]
        }
        parameters[idparsopt] <- exp(log(parameters[idparsopt]) +
                                       stats::rnorm(length(idparsopt),
                                                    0, sigma_temp))
      }

      # Reject if outside the prior
      if (prior_density_function(pars, idparsopt) > 0) {

        # Simulate a new tree, given the proposed parameters. Using DAISIE IW model!!!
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

        accept <- TRUE

        # Calculate the summary statistics for the simulated tree
        if (accept) {
          df_stats <- calc_ss_function(sim_1 = obs_data,
                                       sim_2 = new_sim[[1]])

          # #check if the summary statistics are sufficiently
          for (k in seq_along(df_stats)) {
            if (as.numeric(df_stats[k]) > epsilon[i, k]) {
              accept <- FALSE
            }
          }
        }

        if (accept) {
          number_accepted <- number_accepted + 1
          new_params[[number_accepted]] <- parameters
          sim_list[[number_accepted]] <- new_sim[[1]]
          accepted_weight <- 1
          ss_diff <- rbind(ss_diff,df_stats)

          #calculate the weight
          if (i > 1) {
            accepted_weight <- calc_weight(previous_weights,
                                           previous_params,
                                           parameters,
                                           sigma_temp,
                                           prior_density_function,
                                           idparsopt)
          }

          new_weights[number_accepted] <- accepted_weight

          if ((number_accepted) %%
              (number_of_particles / print_frequency) == 0) {
            cat("**")
            utils::flush.console()
          }
        }
      }

      #convergence if the acceptance rate gets too low
      tried <- tried + 1
      if (tried > (1 / stop_rate) & n_iter > 4) {

        if ((number_accepted / tried) < stop_rate) {
          stoprate_reached <- TRUE
          break
        }
      }
    }

    ss_diff_list[[i]] <- ss_diff

    if (stoprate_reached == FALSE) {
      epsilon[i + 1, ] <- apply(ss_diff, 2, quantile, probs = 0.5)
    }

    ABC <- c()
    for (k in seq_along(new_params)) {
      add <- c()
      for (m in seq_along(parameters)) {
        add <- c(add, new_params[[k]][m])
      }
      ABC <- rbind(ABC, add)
    }
    ABC_list[[i]] <- ABC

    if (stoprate_reached) {
      break
    }
  }
  message("tried times: ", tried)

  output <- list(sim_list = sim_list,
                 ABC = ABC_list,
                 n_iter = n_iter,
                 epsilon = epsilon,
                 obs_sim = obs_data,
                 ss_diff_list = ss_diff_list)
  return(output)
}
