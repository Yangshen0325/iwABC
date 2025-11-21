
#### check if it's for only ABC or ABC+MLE
# only ABC, simulated data use island age=20
# ABC+MLE, simulated data use island age=5


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


ABC_SMC_iw_par_lpi <- function(
     obs_data_ss,
     init_epsilon_values,
     prior_generating_function,
     prior_density_function,
     number_of_particles,
     print_frequency,
     sigma,
     sigma_decay          = 0.25,
     sigma_floor          = 0.02,
     stop_rate,
     num_iterations,
     idparsopt,
     pars,
     ss_set,
     param_set,
     enable_checkpoint    = TRUE,
     checkpoint_dir       = "checkpoints_lpi",
     resume_from          = 0,
     checkpoint_path      = NULL,
    #
     eps_q_early          = 0.90,
     eps_q_mid            = 0.80,
     eps_q_late           = 0.75,
     eps_switch1          = 5,           # i <= 5 use eps_q_early
     eps_switch2          = 15            # 5 < i <= 15 use eps_q_mid
    #
    # num_threads = 1
) {




# Initialise epsilon, weight, saving space --------------------------------

  # Generate a matrix with epsilon values
  epsilon <- matrix(NA_real_, nrow = num_iterations + 1, ncol = length(init_epsilon_values))
  epsilon[1, ] <- init_epsilon_values

  # Initialise weights
  new_weights <- c()
  new_params <- list()
  previous_weights <- c()
  previous_params  <- list()

  indices <- 1:number_of_particles

  n_iter <- 0L

  # Initialize the output lists
  ABC_list <- list()
  sim_seed_list <- list() # keep the seed, generate new_sim later to save memory
  ss_diff_list <- list()


# From checkpoint if needed -----------------------------------------------

  if (resume_from > 0 || !is.null(checkpoint_path)) {
    if (is.null(checkpoint_path)) {
      if (is.na(param_set)) stop("resume_from needs param_set to locate checkpoint directory.")
      checkpoint_path <- file.path(checkpoint_dir, sprintf("chk_lpi_set%04d_iter%02d.rds", param_set, resume_from))
    }
    if (!file.exists(checkpoint_path)) {
      stop("checkpoint doesn't exist: ", checkpoint_path)
    }
    ck <- readRDS(checkpoint_path)

    i_start          <- ck$iter_completed + 1L
    ABC_list         <- ck$ABC_list
    epsilon          <- ck$epsilon
    previous_params  <- ck$previous_params
    previous_weights <- ck$previous_weights
    ss_diff_list     <- ck$ss_diff_list
    sim_seed_list  <- ck$sim_seed_list

    # normalize weights & sanity
    if (!length(previous_weights) ||
        length(previous_weights) != length(previous_params) ||
        any(!is.finite(previous_weights)) || sum(previous_weights) <= 0) {
      previous_weights <- rep(1/length(previous_params), length(previous_params))
    } else {
      previous_weights <- previous_weights / sum(previous_weights)
    }

    resuming_now <- TRUE
    n_iter       <- ck$iter_completed
    message(sprintf("[ABC] Resumed from iter %d using %s", ck$iter_completed, checkpoint_path))
  } else {
    i_start <- 1L
    resuming_now <- FALSE
  }

  # checkpoint directory
  if (enable_checkpoint) {
    dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
  }


# Iterate start -----------------------------------------------------------


  for (i in i_start:num_iterations) {

    ss_diff <- matrix(NA_real_,
                      nrow = number_of_particles,
                      ncol = length(init_epsilon_values))

    n_iter <- n_iter + 1L
    cat("\nGenerating Particles for iteration\t", i, "\n")
    cat("0--------25--------50--------75--------100\n")
    cat("*")
    utils::flush.console()

    sigma_temp <- max(sigma * exp(-sigma_decay * (i - 1)), sigma_floor)

    # Reset `new_weight` and `new_params`
    if (i > 1L) {
      if (resuming_now && i == i_start) {
        # keep restored previous_* as-is
      } else {
        sw <- sum(new_weights)
        if (!length(new_weights) || !is.finite(sw) || sw <= 0) {
          # fallback: uniform weights across accepted new_params
          previous_weights <- rep(1 / length(new_params), length(new_params))
        } else {
          previous_weights <- new_weights / sw
        }
        new_weights <- c()
        previous_params <- new_params
        new_params <- list()
      }
    }

    stoprate_reached <- FALSE
    tried <- 0L
    number_accepted <- 0L
    sim_seed <- integer(0)

    while (number_accepted < number_of_particles) {

      block_size <- number_of_particles - number_accepted
      if (tried > 0L) {
        block_size <- floor(block_size * tried / (1 + number_accepted))
      } else {
        block_size <- floor(block_size)
      }
      if (block_size <= 0L) block_size <- 1L

      parameter_list <- list()
        # In this initial step, generate parameters from the prior
      if (i == 1L) {
        for (np in 1:block_size) {
          parameter_list[[np]] <- prior_generating_function(pars = pars, idparsopt = idparsopt)
        }
       } else {
          #if not in the initial step, generate parameters
          #from the weighted previous distribution:

          # Sample an index from the previous weights, and use the corresponding parameters as a baseline
      #    index <- sample(x = indices, size = 1, prob = previous_weights)
         if (i > 1L) {
           indices <- seq_len(length(previous_params))
         }
          random_indices <- sample(x = indices, size = block_size, prob = previous_weights, replace = TRUE)

          for (np in 1:block_size) {
            local_parameters <- previous_params[[random_indices[np]]]
            local_parameters[idparsopt] <- exp(
              log(local_parameters[idparsopt]) +
                stats::rnorm(length(idparsopt), mean = 0, sd = sigma_temp)
            )
            parameter_list[[np]] <- local_parameters
          }
      }

      res <- vector("list", length(parameter_list))
      for (r in seq_along(parameter_list)) {
        res[[r]] <- process_particle(
          par_values              = parameter_list[[r]],
          prior_density_function  = prior_density_function,
          idparsopt               = idparsopt,
          obs_data_ss             = obs_data_ss,
          epsilon_values          = epsilon[i, ]
        )
      }

      tried <- tried + length(res)

      for (l in seq_along(res)) {
        if (!is.list(res[[l]]) || is.null(res[[l]]$accept)) next
        if (isTRUE(res[[l]]$accept)) {
          # Update the number of accepted particles
          number_accepted <- number_accepted + 1

          # Store the accepted particles
          new_params[[number_accepted]] <- res[[l]]$parameters

          # Store the difference of summary statistics
          ss_diff[number_accepted, ] <- unlist(res[[l]]$df_stats)

          # Store the seed used for simulation
          sim_seed[number_accepted] <- res[[l]]$sim_seed

          accepted_weight <- 1
          # Calculate the weight
          if (i > 1L) {
            accepted_weight <- calc_weight(
              weights = previous_weights,
              particles  = previous_params,
              current       = res[[l]]$parameters,
              sigma      = sigma_temp,
              prior_density_function = prior_density_function,
              idparsopt        = idparsopt
            )
          }
          new_weights[number_accepted] <- accepted_weight

          # Print(**) out every certain number of particles, showing the progress on screen
          # chunk <- max(1L, floor(number_of_particles / print_frequency))
          # if ((number_accepted %% chunk) == 0L) {
          #   cat("**"); utils::flush.console()
          # }
        }
        if (number_accepted >= number_of_particles) break
      }

      # If the stopping condition is met, the loop exits early using `break`
      if (tried > (1 / stop_rate) & n_iter > 4L) {# it checks only after least 5 iterations
        if ((number_accepted / tried) < stop_rate) {
          stoprate_reached <- TRUE
          break
        }
      }
      # Clean up the memory
      block_size <- NULL
      parameter_list <- NULL
      res <- NULL

      #if ((tried %% 100) == 0L) gc()
    }

    if(i %% 5 == 0) gc()

    ss_diff_list[[i]] <- ss_diff
    sim_seed_list[[i]] <- sim_seed

    # Update the epsilon values for the next iteration
    if (!stoprate_reached) {
      for (j in 1:ncol(ss_diff)) {
        if (sd(ss_diff[, j], na.rm = TRUE) > 0) {
          q <- if (i <= eps_switch1) eps_q_early else if (i <= eps_switch2) eps_q_mid else eps_q_late
          epsilon[i + 1, j] <- stats::quantile(ss_diff[, j], probs = q, na.rm = TRUE)
        } else {
          epsilon[i + 1, j] <- max(ss_diff[, j], na.rm = TRUE)
        }
      }
    } else {
      epsilon[i + 1, ] <- epsilon[i, ]
    }

    ABC_list[[i]] <- do.call(rbind, new_params)
    message("tried times: ", tried)


    if (enable_checkpoint) {
      if (is.na(param_set)) {
        fn <- file.path(checkpoint_dir, sprintf("chk_lpi_iter%02d.rds", i))
      } else {
        fn <- file.path(checkpoint_dir, sprintf("chk_lpi_set%04d_iter%02d.rds", param_set, i))
      }
      checkpoint <- list(
        iter_completed  = i,
        ABC_list        = ABC_list,
        epsilon         = epsilon,
        previous_params = new_params,
        previous_weights= new_weights,
        ss_diff_list    = ss_diff_list,
        sim_seed_list = sim_seed_list
      )
      saveRDS(checkpoint, fn)
    }

    if (stoprate_reached) {
      message(sprintf("[ABC] Early stop at iter %d (accept rate < stop_rate).", i))
      break
    }
 }

    output <- list(
      ABC      = ABC_list,
      epsilon  = epsilon,
      ss_diff_list = ss_diff_list,
      sim_seed_list = sim_seed_list
    )
  return(output)
}

#' @keywords internal
process_particle <- function(par_values,
                             prior_density_function,
                             idparsopt,
                             obs_data_ss,
                             epsilon_values) {

  # 1) Prior gate
  prior_val <- prior_density_function(par_values, idparsopt)
  if (is.na(prior_val) || is.infinite(prior_val) || prior_val <= 0) {
    return(list(accept = FALSE,
                df_stats = NA_real_,
                sim_seed = NA_integer_,
                parameters = par_values))
  }

  # Generate random seed and store them
  sim_seed <- sample(1:1000000, 1)
  set.seed(sim_seed)
  # Simulate a new tree, given the proposed parameters. Using DAISIE IW model!!!
  new_sim <- DAISIEiwsim::DAISIE_sim_cr(
    time = 20,
    M = 1000,
    pars = as.numeric(c(par_values[1], par_values[2], par_values[3], par_values[4], par_values[5])),
    replicates = 1,
    divdepmodel = "IW",
    nonoceanic_pars = c(0, 0),
    sample_freq  = Inf,
    plot_sims = FALSE,
    verbose = FALSE,
    cond = 1,
    rcpp = TRUE
  )


  # Calculate the summary statistics for the simulated tree
  new_sim_ss_all <- calc_all_stats(sim = new_sim[[1]])

  new_sim <- NULL

  if (ss_set == 0) {            # All
    new_sim_ss <- new_sim_ss_all

  } else if (ss_set == 1) {     # Species richness
    new_sim_ss <- new_sim_ss_all[c("num_nonend_sim", "num_sington_sim", "num_multi_sim")]

  } else if (ss_set == 2) {     # NLTT
    new_sim_ss <- new_sim_ss_all[c("nonend_nltt", "singleton_nltt", "multi_nltt")]

  } else if (ss_set == 3) {     # Clade distribution
    new_sim_ss <- new_sim_ss_all[c("first_clade_size",
                                   "prop_largest_clade",
                                   "rank_largest_clade",
                                   "clade_evenness")]

  } else if (ss_set == 4) {     # Colonisation + NLTT
    new_sim_ss <- new_sim_ss_all[c("nonend_nltt", "singleton_nltt", "multi_nltt",
                                   "sim_ct_sd", "num_colon")]

  } else {
    stop("Invalid value for ss_set. Only 0–4 are supported.")
  }

  df_stats <- unlist(abs(new_sim_ss - obs_data_ss))

  accept <- TRUE

  num_accepted_stats <- df_stats <= epsilon_values
  if (sum(num_accepted_stats) != length(df_stats)) accept <- FALSE

  out <- list("accept" = accept,
              "df_stats" = df_stats,
              "sim_seed" = sim_seed,
              "parameters" = par_values)

  return(out)
}

