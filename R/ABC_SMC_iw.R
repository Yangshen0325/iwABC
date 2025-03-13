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
#' @param print_frequency The frequency of printing the progress.
#' @param sigma Standard deviation of the perturbation distribution.
#' @param stop_rate A numeric value which is the boundary to stop the algorithm.
#' @param num_iterations The maximum number of iterations.
#' @param idparsopt The id of the parameters that need to be inferred, the others
#'  are fixed.
#' @param pars A vector of parameters.
#' @param ss_set A numeric indicates which set of summary statistics that
#'  are used to calculate the distance.
#'  @export


ABC_SMC_iw <- function(
    obs_data,
    calc_ss_function,
    init_epsilon_values,
    prior_generating_function,
    prior_density_function,
    number_of_particles,
    print_frequency,
    sigma,
    stop_rate,
    num_iterations,
    idparsopt,
    pars,
    ss_set
) {

  # Generate a matrix with epsilon values, assuming after 20 iterations, convergence occurs (Shu)
  epsilon <- matrix(nrow = 20, ncol = length(init_epsilon_values))
  epsilon[1, ] <- init_epsilon_values

  # Initialise weights
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

    print_frequency <- print_frequency

    tried <- 0

    number_accepted <- 0

    sigma_temp <- sigma * exp(-0.2 * (i - 1))

    # Reset `new_weight` and `new_params`
    if (i > 1) {
      #normalize the weights and store them as previous weights.
      previous_weights <- new_weights / sum(new_weights)
      new_weights <- c() #remove all currently stored weights
      previous_params <- new_params #store found params
      new_params <- list(c(seq_along(pars))) #clear new params
    }

    stoprate_reached <- FALSE

    while (number_accepted < number_of_particles) {

      # In this initial step, generate parameters from the prior
      if (i == 1) {
        parameters <- prior_generating_function(pars = pars,
                                                idparsopt = idparsopt)
      } else {
        #if not in the initial step, generate parameters
        #from the weighted previous distribution:

        # Sample an index from the previous weights, and use the corresponding parameters as a baseline
        index <- sample(x = indices, size = 1,
                        replace = TRUE, prob = previous_weights)
        for (p_index in seq_along(parameters)) {
          parameters[p_index] <- previous_params[[index]][p_index]
        }

        # Perturb the parameters
        parameters[idparsopt] <- exp(log(parameters[idparsopt]) +
                                       stats::rnorm(length(idparsopt), # add random noise
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
          df_stats <- calc_ss_function(sim1 = obs_data,
                                       sim2 = new_sim[[1]],
                                       ss_set = ss_set)

          # Check if the summary statistics meet the criteria, yes-accept, any of them is larger than epsilon-reject
          for (k in seq_along(df_stats)) {
            if (as.numeric(df_stats[k]) > epsilon[i, k]) {
              accept <- FALSE
            }
          }
        }

        if (accept) {
          # Update the number of accepted particles
          number_accepted <- number_accepted + 1

          # Store the accepted particles
          new_params[[number_accepted]] <- parameters

          # Store the accepted simulations
          sim_list[[number_accepted]] <- new_sim[[1]]

          accepted_weight <- 1

          # Store the difference of summary statistics
          ss_diff <- rbind(ss_diff, df_stats)

          # Calculate the weight
          if (i > 1) {
            accepted_weight <- calc_weight(previous_weights,
                                           previous_params,
                                           parameters,
                                           sigma_temp,
                                           prior_density_function,
                                           idparsopt)
          }

          new_weights[number_accepted] <- accepted_weight

          # Print(**) out every certain number of particles, showing the progress on screen
          # if ((number_accepted) %%
          #     (number_of_particles / print_frequency) == 0) {
          #   cat("**")
          #   utils::flush.console()
          # }
        }
      }

      # If the stopping condition is met, the loop exits early using `break`
      tried <- tried + 1
      if (tried > (1 / stop_rate) & n_iter > 4) {# it checks only after least 5 iterations

        if ((number_accepted / tried) < stop_rate) {
          stoprate_reached <- TRUE
          break
        }
      }
    }

    message("tried times: ", tried)

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


  output <- list(sim_list = sim_list,
                 ABC = ABC_list,
                 n_iter = n_iter,
                 epsilon = epsilon,
                 obs_sim = obs_data,
                 ss_diff_list = ss_diff_list)
  return(output)
}
