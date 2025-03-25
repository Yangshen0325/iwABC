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
#' @param num_threads number of threads
#'  @export


ABC_SMC_iw_par <- function(
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
    ss_set,
    num_threads = 1
) {

  # Generate a matrix with epsilon values
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

    ss_diff <- matrix(NA,
                      nrow = number_of_particles,
                      ncol = length(init_epsilon_values))

    n_iter <- n_iter + 1

    cat("\nGenerating Particles for iteration\t", i, "\n")
    cat("0--------25--------50--------75--------100\n")
    cat("*")
    utils::flush.console()

    sigma_temp <- sigma * exp(-0.5 * (i - 1)) # old value is 0.2

    # Reset `new_weight` and `new_params`
    if (i > 1) {
      #normalize the weights and store them as previous weights.
      previous_weights <- new_weights / sum(new_weights)
      new_weights <- c() #remove all currently stored weights
      previous_params <- new_params #store found params
      new_params <- list(c(seq_along(pars))) #clear new params
    }

    stoprate_reached <- FALSE
    tried <- 0
    number_accepted <- 0

    while (number_accepted < number_of_particles) {

      block_size <- number_of_particles - number_accepted
      if (tried > 0)
        block_size <- block_size * tried / (1 + number_accepted) # 1 / (number_accepted / tried)

      block_size <- floor(block_size)

      parameter_list <- list()
      random_indices <- sample(x = indices,
                               size = block_size,
                               prob = previous_weights,
                               replace = TRUE)
      for (np in 1:block_size) {
        # In this initial step, generate parameters from the prior
        if (i == 1) {
          parameter_list[[np]] <- prior_generating_function(pars = pars,
                                                            idparsopt = idparsopt)
        } else {
          #if not in the initial step, generate parameters
          #from the weighted previous distribution:

          # Sample an index from the previous weights, and use the corresponding parameters as a baseline
      #    index <- sample(x = indices, size = 1, prob = previous_weights)

          local_parameters <- previous_params[[random_indices[np]]]

          # Perturb the parameters
          local_parameters[idparsopt] <- exp(log(local_parameters[idparsopt]) +
                                               stats::rnorm(length(idparsopt), # add random noise
                                                            0, sigma_temp))
          parameter_list[[np]] <- local_parameters
        }
      }

      want_to_debug <- FALSE

      if (want_to_debug) {
        res <- list()
        for (r in 1:length(parameter_list)) {
          res[[r]] <- process_particle(parameter_list[[r]],
                                       prior_density_function,
                                       idparsopt,
                                       calc_ss_function,
                                       obs_data,
                                       epsilon[i, ])
        }

      } else {
        res <- parallel::mclapply(parameter_list,
                                  process_particle,
                                  prior_density_function = prior_density_function,
                                  idparsopt = idparsopt,
                                  calc_ss_function = calc_ss_function,
                                  obs_data = obs_data,
                                  epsilon_values = epsilon[i, ],
                                  mc.cores = num_threads,
                                  mc.preschedule = FALSE,
                                  mc.allow.recursive = FALSE)
      }

      tried <- tried + length(res)

      for (l in 1:length(res)) {
        if (res[[l]]$accept == TRUE) {
          # Update the number of accepted particles
          number_accepted <- number_accepted + 1

          # Store the accepted particles
          new_params[[number_accepted]] <- res[[l]]$parameters

          # Store the accepted simulations
          sim_list[[number_accepted]] <- res[[l]]$sim[[1]]

          accepted_weight <- 1
          # Store the difference of summary statistics
          ss_diff[number_accepted, ] <- res[[l]]$df_stats

          # Calculate the weight
          if (i > 1) {
            accepted_weight <- calc_weight(previous_weights,
                                           previous_params,
                                           res[[l]]$parameters,
                                           sigma_temp,
                                           prior_density_function,
                                           idparsopt)
          }

          new_weights[number_accepted] <- accepted_weight

          # Print(**) out every certain number of particles, showing the progress on screen
          if ((number_accepted) %%
              (number_of_particles / print_frequency) == 0) {
            cat("**")
            utils::flush.console()
          }
        }

        if (number_accepted >= number_of_particles) break
      }

      # If the stopping condition is met, the loop exits early using `break`
      if (tried > (1 / stop_rate) & n_iter > 4) {# it checks only after least 5 iterations

        if ((number_accepted / tried) < stop_rate) {
          stoprate_reached <- TRUE
          break
        }
      }
    }


    ss_diff_list[[i]] <- ss_diff

    if (stoprate_reached == FALSE) {
      epsilon[i + 1, ] <- apply(ss_diff, 2, stats::quantile, probs = 0.95)
    }

    ABC_list[[i]] <- do.call(rbind, new_params)

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

#' @keywords internal
process_particle <- function(par_values,
                             prior_density_function,
                             idparsopt,
                             calc_ss_function,
                             obs_data,
                             epsilon_values) {

  if (prior_density_function(par_values, idparsopt) < 0) {
    out <- list("accept" = FALSE)
    return(out)
  }

  # Simulate a new tree, given the proposed parameters. Using DAISIE IW model!!!
  new_sim <- DAISIE::DAISIE_sim_cr(
    time = 5,
    M = 1000,
    pars = as.numeric(c(par_values[1], par_values[2], par_values[3], par_values[4], par_values[5])),
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

  accept <- TRUE

  num_accepted_stats <- df_stats <= epsilon_values
  if (sum(num_accepted_stats) != length(df_stats)) accept <- FALSE

  out <- list("accept" = accept,
              "df_stats" = df_stats,
              "sim" = new_sim,
              "parameters" = par_values)

  return(out)
}
