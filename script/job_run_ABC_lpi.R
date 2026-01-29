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

## --- Auto-detect latest checkpoint for this param_set ---
checkpoint_dir  <- file.path("newSimABC_lpi_firstTen", sprintf("checkpoints_lpi_set_%04d", param_set))
resume_from     <- 0
checkpoint_path <- NULL

if (dir.exists(checkpoint_dir)) {
  # files like: chk_set0012_iter08.rds
  pats <- sprintf("^chk_lpi_set%04d_iter(\\d{2})\\.rds$", param_set)
  files <- list.files(checkpoint_dir, pattern = pats, full.names = TRUE)
  if (length(files) > 0) {
    # extract iteration numbers
    iters <- as.integer(sub(sprintf("^.*chk_lpi_set%04d_iter(\\d{2})\\.rds$", param_set), "\\1", files))
    last_idx <- which.max(iters)
    resume_from     <- iters[last_idx]     # e.g. 8  → will continue at 9
    checkpoint_path <- files[last_idx]
    message(sprintf("[AUTO-RESUME] Found checkpoint: iter %d at %s",
                    resume_from, checkpoint_path))
  } else {
    message("[AUTO-RESUME] No checkpoint files found; starting from iter 1.")
  }
} else {
  message("[AUTO-RESUME] No checkpoint dir; starting from iter 1.")
}


idparsopt <- c(idparsopt_lac, idparsopt_mu, idparsopt_K, idparsopt_gam, idparsopt_laa)

if (length(idparsopt) == 0) {
  stop("No parameters selected to optimize (all flags were 0).")
}

library(iwABC)
library(DAISIEiwsim)

saveOrNot <- FALSE

iwABC::run_ABC_par_lpi(
  param_set = param_set,
  idparsopt = idparsopt,
  saveOrNot = saveOrNot,
  ss_set = ss_set,
  number_of_particles = 500,
  num_iterations = 15,
  stop_rate = 1e-7,

  # prior functions
  prior_generating_function = prior_gen_onlyABC,
  prior_density_function = prior_dens_onlyABC,

  # NEW: pass resume controls through
  resume_from    = resume_from,
  checkpoint_path= checkpoint_path
)


