
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
run_ABC_par_spi <- function(param_set,
                        idparsopt,
                        ss_set = 0,
                        number_of_particles = 500,
                        num_iterations = 10,
                        print_frequency = 20,
                        sigma = 0.05,
                        stop_rate = 1e-7,
                        saveOrNot = FALSE,
                        prior_generating_function,
                        prior_density_function,
                        resume_from    = 0,
                        checkpoint_path= NULL) {
  #start_of_file_name){

  # Read corresponding parameter space and summary stats of observed data
  param_space <- file.path("~/iwABCdata/single_pars_spi", paste0("ABC_pars_spi_", param_set, ".rds"))
  obs_ss_data  <- file.path("~/iwABCdata/single_obs_ss_spi", paste0("obs_ss_spi_", param_set, ".rds"))

  obs_sim_pars <- readRDS(param_space)
  obs_sim_ss <- readRDS(obs_ss_data)

  # --- Skip if observed summary stats are empty ---
  if (nrow(obs_sim_ss) == 0) {
    message(sprintf("Skipping param_set %d: obs summary stats are empty.", param_set))
    return(NULL)
  }

  # set seed and print out
  seed <- (sample.int(.Machine$integer.max, 1L) + param_set) %% .Machine$integer.max
  if (seed == 0L) seed <- 1L
  set.seed(seed)
  message("Running param set: ", param_set)
  message("seed: ", seed)

  # Choose the summary statistics set
  if(ss_set == 0){ # all
    init_epsilon <- c(50,   # 1 num_nonend,
                      50,   # 2 num_singleton,
                      1000,  # 3 num_multi,
                      100,   # 4 nonend_nltt,
                      200,   # 5 singleton_nltt,
                      15000,  # 6 multi_nltt,
                      1000,   # 7 first_clade_diff
                      1,    # 8 prop_largest_clade_diff,
                      50,   # 9 rank_largest_clade_diff,
                      1,    # 10 clade evenness
                      50,    # 11 sd_colon_time,
                      50)   # 12 num_colon

  } else if (ss_set == 1){  # Species Richness
    use_obs_ss <- obs_sim_ss[c("num_nonend_sim", "num_sington_sim", "num_multi_sim")]
    obs_sim_ss <- use_obs_ss
    init_epsilon <- c(50, # 1 num_nonend,
                      50, # 2 num_singleton,
                      1000 # 3 num_multi,
    )
    # phylogenetic: nltt+sd ##########
  } else if (ss_set == 2){  # NLTT
    use_obs_ss <- obs_sim_ss[c("nonend_nltt", "singleton_nltt", "multi_nltt")]
    obs_sim_ss <- use_obs_ss
    init_epsilon <- c(100, #  nonend_nltt,
                      200, # singleton_nltt,
                      15000) # multi_nltt,

  } else if(ss_set == 3){ # Clade Distribution
    use_obs_ss <- obs_sim_ss[c("first_clade_size", "prop_largest_clade", "rank_largest_clade", "clade_evenness")]
    obs_sim_ss <- use_obs_ss
    init_epsilon <- c(1000, #  first_clade_diff
                      1, # prop_largest_clade_diff,
                      50, #  rank_largest_clade_diff,
                      1)   # clade evenness

  } else if(ss_set == 4){ # Colonisation Related + NLTT
    use_obs_ss <- obs_sim_ss[c("nonend_nltt", "singleton_nltt", "multi_nltt", "sim_ct_sd", "num_colon")]
    obs_sim_ss <- use_obs_ss
    init_epsilon <- c(100, #  nonend_nltt,
                      200, # singleton_nltt,
                      15000, # multi_nltt,
                      50, # sd_colon_time,
                      50) # num_colon

    } else {
    stop("Invalid value for ss_set. Only 0,1,2,3,4 are supported.") # will have more options in the future
  }

  if (length(init_epsilon) != length(obs_sim_ss)) {
    stop(sprintf("Length mismatch: init_epsilon=%d vs obs_sim_ss=%d",
                 length(init_epsilon), length(obs_sim_ss)))
  }

  print_frequency <- max(1L, min(print_frequency, number_of_particles))

  # Run ABC-SMC
  abc <- ABC_SMC_iw_par_spi(obs_data_ss = obs_sim_ss,
                        init_epsilon_values = init_epsilon,
                        prior_generating_function = prior_generating_function,
                        prior_density_function = prior_density_function,
                        number_of_particles = number_of_particles,
                        print_frequency = print_frequency,
                        sigma = sigma,
                        sigma_decay             = 0.25,
                        sigma_floor             = 0.02,
                        stop_rate = stop_rate,
                        num_iterations = num_iterations,
                        idparsopt = idparsopt,
                        pars = as.numeric(obs_sim_pars[1:5]),
                        ss_set = ss_set,
                        param_set = param_set,
                        enable_checkpoint       = TRUE,
                        checkpoint_dir          = file.path("newSimABC_spi_firstTen", sprintf("checkpoints_spi_set_%04d", param_set)),
                        resume_from             = resume_from,         # <- pass through a number or keep default 0
                        checkpoint_path         = checkpoint_path )     # <- NULL normally; set if you resume from a custom file

  if(saveOrNot == TRUE){
    save_output(output = abc,
                param_set = param_set,
                ss_set = ss_set)

  }else{
    return(abc)
  }
}
