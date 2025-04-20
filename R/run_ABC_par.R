
#' Run ABC-SMC with DAISIE-IW data
#' @param param_set A numeric of a specific line of parameter set in parameter space
#' @param idparsopt A vector of positions of the parameters that be optimized.
#' @param number_of_particles The number of particles in each iteration.
#' @param num_iterations The maximum number of iterations.
#' @param print_frequency The frequency of printing out the progress.
#' @param sigma The standard deviation of the normal distribution used to generate the proposal distribution.
#' @param stop_rate The stopping rate of the ABC-SMC algorithm (the process should stop if the success rate falls below it)
#' @param num_threads number of threads



#' @export
run_ABC_par <- function(param_set,
                        idparsopt,
                        ss_set = 0,
                        number_of_particles = 100,
                        num_iterations = 20,
                        print_frequency = 20,
                        sigma = 0.05,
                        stop_rate = 1e-5,
                        saveOrNot = TRUE,
                        num_threads = 1){
                        #start_of_file_name){

  # Read data, use file.path() to make them system-independent:
  param_space <- utils::read.csv(file.path("data", "parameter_space.csv"))
  iw_observations <- readRDS(file.path("data", "iw_observations.rds"))

  # set seed and print out
  seed <- as.integer(Sys.time()) %% 1000000L * param_set
  set.seed(seed)
  message("Running param set: ", param_set)
  message("seed: ", seed)

  # Extract the parameters and corresponding simulated(observed/empirical) data
  obs_sim_pars <- param_space[param_set, ]
  obs_sim <- iw_observations[[param_set]]

  # Choose the summary statistics set
  if(ss_set == 0){ # all
    init_epsilon <- c(50, 50, 100, 100, 100, 200, 10, 50, 100, 10, 20, 10, 100)
  } else if (ss_set == 1){  #
    init_epsilon <- c(200,50,50,50,50)
  } else {
    stop("Invalid value for ss_set. Only 0 and 1 are supported.") # will have more options in the future
  }

  # Run ABC-SMC
  abc <- ABC_SMC_iw_par(obs_data = obs_sim,
                    calc_ss_function = calc_ss_iw,
                    init_epsilon_values = init_epsilon,
                    prior_generating_function = prior_gen,
                    prior_density_function = prior_dens,
                    number_of_particles = number_of_particles,
                    print_frequency = print_frequency,
                    sigma = sigma,
                    stop_rate = stop_rate,
                    num_iterations = num_iterations,
                    idparsopt = idparsopt,
                    pars = as.numeric(obs_sim_pars[1:5]),
                    ss_set = ss_set,
                    num_threads = num_threads)
                    #start_of_file_name = start_of_file_name)

  if(saveOrNot == TRUE){
    save_output(output = abc,
                param_set = param_set,
                ss_set = ss_set)

  }else{
    return(abc)
  }
}
