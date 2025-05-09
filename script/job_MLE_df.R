#!/usr/bin/env Rscript
# ---------------------------------------------------
# mle_job.R   -- compute one MLE for a given parameter set
# ---------------------------------------------------

# Load libraries ------------------------------------------------------------
suppressMessages({
  library(optparse)
  library(iwABC)
  library(parallel)
})

DAISIE::DAISIE_IW_num_threads(1)

outdir <- "~/iwABC/out/mle/"

# 1. parse args ------------------------------------------------------------
option_list <- list(
  make_option(c("--index"),  type="integer"),
  make_option(c("--ncores"), type="integer")
)
opt <- parse_args(OptionParser(option_list=option_list))
i      <- opt$index
ncores <- opt$ncores

# 2. load data -------------------------------------------------------------
parameter_space <- read.csv("~/iwABC/data/parameter_space_rep100_small_k.csv")
iw_obs_flat     <- readRDS("~/iwABC/data/iw_observations_rep100_small_k.rds")

n_total <- nrow(parameter_space)
n_reps  <- 100
n_sets  <- n_total / n_reps
stopifnot(n_sets == as.integer(n_sets),
          i >= 1, i <= n_sets)

if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# 3. slice out the 100 replicates for set i -------------------------------
start_idx <- (i - 1) * n_reps + 1        # 1, 101, 201, …
end_idx   <- i * n_reps                  # 100, 200, 300, …
message(sprintf("Task %2d/%2d → rows %4d:%4d", i, n_sets, start_idx, end_idx))

the_sim   <- iw_obs_flat[start_idx:end_idx]
pars_use  <- parameter_space[start_idx, -ncol(parameter_space)]

# 4. set seed --------------------------------------------------------------
seed_mle <- as.integer(Sys.time()) %% 1e6L * sample(1:10,1)
set.seed(seed_mle)
message("Index=", i, " seed=", seed_mle)

# 5. parallel MLE over 100 replicates --------------------------------------
#    split the_sim (length 100) across ncores
res_list <- mclapply(
  seq_along(the_sim),
  function(j) {
    # before you call get_MLE, print which rep this is:
    message(sprintf(
      "[%s] Starting rep %3d on PID %d",
      format(Sys.time(), "%H:%M:%S"),
      j,
      Sys.getpid()
    ))
    out <- iwABC::get_MLE(the_sim[[j]], pars_use = pars_use)
    message(sprintf(
      "[%s] Finished rep %3d",
      format(Sys.time(), "%H:%M:%S"),
      j
    ))
    out[["sim"]] = i;   # add sim index
    out[["rep"]] = j;   # add rep
    outfile <- file.path(outdir, sprintf("%d_%d.txt", i, j))
    write.table(as.data.frame(out), file = outfile);
    out
  },
  mc.cores            = ncores,
  mc.preschedule      = FALSE,      # better load balancing
  mc.allow.recursive  = FALSE
)
