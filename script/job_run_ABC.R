args <- commandArgs(TRUE) # use command-line arguments passed when executing the script

# Uncomment for local testing:
# args <- c(2, 1, 2, 3, 4, 5)

DAISIE::DAISIE_IW_num_threads(1)

param_set <- as.numeric(args[1])
idparsopt_lac <- as.numeric(args[2])
idparsopt_mu <- as.numeric(args[3])
idparsopt_K <- as.numeric(args[4])
idparsopt_gam <- as.numeric(args[5])
idparsopt_laa <- as.numeric(args[6])
ss_set <- as.numeric(args[7])

idparsopt_all <- c(idparsopt_lac, idparsopt_mu, idparsopt_K, idparsopt_gam, idparsopt_laa)

idparsopt <- idparsopt_all #which(idparsopt_all == 1)

saveOrNot <- TRUE

metadata <- paste0("This is parameter set ", param_set)


library(iwABC)

iwABC::run_ABC_par(
  param_set = as.numeric(args[1]),
  idparsopt = as.numeric(idparsopt),
  saveOrNot = saveOrNot,
  ss_set = 0,
  number_of_particles = 1000,
  num_iterations = 10,
  num_threads = 1,
  stop_rate = 1e-7
  #start_of_file_name = paste0("set_", param_set, "_", ss_set, "_")
)


