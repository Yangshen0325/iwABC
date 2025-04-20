args <- commandArgs(TRUE) # use command-line arguments passed when executing the script

# Uncomment for local testing:
# args <- c(1, 1, 2, 3, 4, 5)

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
  number_of_particles = 500,
  num_iterations = 30,
  num_threads = 8,
  stop_rate = 1e-7
  #start_of_file_name = paste0("set_", param_set, "_")
)


