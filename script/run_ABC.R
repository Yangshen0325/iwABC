
#### 00 observation simulation output, only 1 for testing
# pars that generate the obeservation:
# lac, mu, k, gam,  laa
# 0.4, 0, 20, 0.003, 0.1
library(iwABC)

# the_path <- "~/Downloads/phd_yang/pkgs/iwABC/script/"
the_path <- "~/iwABC/script/"
iw_observations <- readRDS(paste0(the_path, "iw_observations.rds"))
obs_sim <- iw_observations[[1]]

# parameters (or a particle)
parameters <- c(0.4, 0, 20, 0.003, 0.1)

# initial epsilon values
init_epsilon <- c(25, 47, 3, 46, 45, 82, 265, 45, 118, 1.0, 152, 45, 91, 293)

# number if iteration
num_iterations <- 20

# number of particles
number_of_particles <- 500

abc <- ABC_SMC_iw(obs_data = obs_sim,
                  calc_ss_function <- calc_error_all,
                  init_epsilon_values = init_epsilon,
                  prior_generating_function <- prior_gen,
                  prior_density_function <- prior_dens,
                  number_of_particles = number_of_particles,
                  sigma = 0.05,
                  stop_rate = 1e-3,
                  num_iterations = num_iterations,
                  idparsopt = c(1, 2, 3, 4, 5),
                  pars = parameters)


saveRDS(abc, file = paste0(the_path, "abc.rds"))


