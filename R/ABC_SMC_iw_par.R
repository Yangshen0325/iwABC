
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
    epsilon <- matrix(nrow = num_iterations + 1, ncol = length(init_epsilon_values))
    epsilon[1, ] <- init_epsilon_values

    new_weights <- c()
    new_params <- list(c(seq_along(pars)))
    previous_weights <- c()
    previous_params <- list(c(seq_along(pars)))

    indices <- 1:number_of_particles
    n_iter <- 0

    ABC_list <- list()
    sim_list <- list()
    ss_diff_list <- list()

    for (i in 1:num_iterations) {
        ss_diff <- matrix(NA, nrow = number_of_particles, ncol = length(init_epsilon_values))

        n_iter <- n_iter + 1

        cat("\nGenerating Particles for iteration\t", i, "\n")
        cat("0--------25--------50--------75--------100\n")
        cat("*")
        utils::flush.console()

        sigma_temp <- sigma * exp(-0.5 * (i - 1))

        if (i > 1) {
            previous_weights <- new_weights / sum(new_weights)
            new_weights <- c()
            previous_params <- new_params
            new_params <- list(c(seq_along(pars)))
        }

        stoprate_reached <- FALSE
        tried <- 0
        number_accepted <- 0

        while (number_accepted < number_of_particles) {
            block_size <- number_of_particles - number_accepted
            if (tried > 0) {
                block_size <- block_size * tried / (1 + number_accepted)
            }
            block_size <- floor(block_size)

            parameter_list <- list()
            random_indices <- sample(x = indices, size = block_size, prob = previous_weights, replace = TRUE)
            for (np in 1:block_size) {
                if (i == 1) {
                    parameter_list[[np]] <- prior_generating_function(pars = pars, idparsopt = idparsopt)
                } else {
                    local_parameters <- previous_params[[random_indices[np]]]
                    local_parameters[idparsopt] <- exp(
                        log(local_parameters[idparsopt]) +
                            stats::rnorm(length(idparsopt), 0, sigma_temp)
                    )
                    parameter_list[[np]] <- local_parameters
                }
            }

            use_multithreading <- FALSE
            # res <- list()

            if (use_multithreading) {
                res <- parallel::mclapply(
                    parameter_list,
                    process_particle,
                    prior_density_function = prior_density_function,
                    idparsopt = idparsopt,
                    calc_ss_function = calc_ss_function,
                    obs_data = obs_data,
                    epsilon_values = epsilon[i, ],
                    mc.cores = num_threads,
                    mc.preschedule = FALSE,
                    mc.allow.recursive = FALSE
                )

                # stream the parallel results
                for (l in 1:length(res)) {
                    tried <- tried + 1
                    if (isTRUE(res[[l]]$accept)) {
                        number_accepted <- number_accepted + 1
                        new_params[[number_accepted]] <- res[[l]]$parameters
                        ss_diff[number_accepted, ] <- res[[l]]$df_stats
                        sim_list[[number_accepted]] <- res[[l]]$sim[[1]]

                        accepted_weight <- 1
                        if (i > 1) {
                            accepted_weight <- calc_weight(
                                previous_weights,
                                previous_params,
                                res[[l]]$parameters,
                                sigma_temp,
                                prior_density_function,
                                idparsopt
                            )
                        }
                        new_weights[number_accepted] <- accepted_weight

                        if ((number_accepted) %% (number_of_particles / print_frequency) == 0) {
                            cat("**")
                            utils::flush.console()
                        }
                    }
                    if (number_accepted >= number_of_particles) break
                }
            } else {
                # stream each particle immediately
                for (r in 1:length(parameter_list)) {
                    tmp <- process_particle(
                        parameter_list[[r]],
                        prior_density_function,
                        idparsopt,
                        calc_ss_function,
                        obs_data,
                        epsilon[i, ]
                    )
                    tried <- tried + 1

                    if (isTRUE(tmp$accept)) {
                        number_accepted <- number_accepted + 1
                        new_params[[number_accepted]] <- tmp$parameters
                        ss_diff[number_accepted, ] <- tmp$df_stats

                        # only accepted sims are kept
                        sim_list[[number_accepted]] <- tmp$sim[[1]]

                        accepted_weight <- 1
                        if (i > 1) {
                            accepted_weight <- calc_weight(
                                previous_weights,
                                previous_params,
                                tmp$parameters,
                                sigma_temp,
                                prior_density_function,
                                idparsopt
                            )
                        }
                        new_weights[number_accepted] <- accepted_weight

                        if ((number_accepted) %% (number_of_particles / print_frequency) == 0) {
                            cat("**")
                            utils::flush.console()
                        }
                    }

                    # free temp object
                    rm(tmp)
                    if (tried %% 100 == 0) {
                        gc(FALSE)
                    }

                    if (number_accepted >= number_of_particles) break
                }
            }

            if (tried > (1 / stop_rate) & n_iter > 4) {
                if ((number_accepted / tried) < stop_rate) {
                    stoprate_reached <- TRUE
                    break
                }
            }
        }

        ss_diff_list[[i]] <- ss_diff

        if (stoprate_reached == FALSE) {
            for (j in 1:ncol(ss_diff)) {
                if (sd(ss_diff[, j]) > 0) {
                    epsilon[i + 1, j] <- stats::quantile(ss_diff[, j], probs = 0.75)
                } else {
                    epsilon[i + 1, j] <- max(ss_diff[, j])
                }
            }
        }

        ABC_list[[i]] <- do.call(rbind, new_params)

        if (stoprate_reached) break
    }

    message("tried times: ", tried)

    output <- list(
        sim_list = sim_list,
        ABC = ABC_list,
        n_iter = n_iter,
        epsilon = epsilon,
        obs_sim = obs_data,
        ss_diff_list = ss_diff_list
    )
    return(output)
}

#' @keywords internal
process_particle <- function(
    par_values,
    prior_density_function,
    idparsopt,
    calc_ss_function,
    obs_data,
    epsilon_values
) {
    if (prior_density_function(par_values, idparsopt) < 0) {
        out <- list("accept" = FALSE)
        return(out)
    }

    new_sim <- DAISIE::DAISIE_sim_cr(
        time = 20,
        M = 1000,
        pars = as.numeric(c(par_values[1], par_values[2], par_values[3], par_values[4], par_values[5])),
        replicates = 1,
        divdepmodel = "IW",
        nonoceanic_pars = c(0, 0),
        sample_freq = Inf,
        plot_sims = FALSE,
        verbose = FALSE,
        cond = 1
    )

    df_stats <- calc_ss_function(sim1 = obs_data, sim2 = new_sim[[1]], ss_set = ss_set)

    accept <- all(df_stats <= epsilon_values)

    if (!accept) {
        # do not return sim on reject
        rm(new_sim)
        gc(FALSE)
        return(list(accept = FALSE, df_stats = df_stats, parameters = par_values))
    }

    list(accept = TRUE, df_stats = df_stats, sim = new_sim, parameters = par_values)
}
