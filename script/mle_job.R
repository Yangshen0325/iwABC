#!/usr/bin/env Rscript
# ---------------------------------------------------
# mle_job.R   -- compute one MLE for a given parameter set
# ---------------------------------------------------

# Load libraries ------------------------------------------------------------
#suppressPackageStartupMessages({
  library(DAISIE)
  library(optparse)
  library(iwABC)
#})

# Just in case: force single-threaded DAISIE_IW
DAISIE::DAISIE_IW_num_threads(1)

# 1. parse args ------------------------------------------------------------
option_list <- list(
  make_option(c("--index"),  type="integer"),   # SLURM_ARRAY_TASK_ID
  make_option(c("--tasks"),  type="integer")    # SLURM_ARRAY_TASK_COUNT
)
opt <- parse_args(OptionParser(option_list=option_list))
index <- opt$index
tasks <- opt$tasks
message("index: ", index)
message("task count: ", tasks)

# 2. load data -------------------------------------------------------------
parameter_space <- read.csv("~/iwABC/data/parameter_space_rep100_small_k.csv")
iw_obs_flat     <- readRDS("~/iwABC/data/iw_observations_rep100_small_k.rds")

n_total <- nrow(parameter_space)
n_reps  <- 100
n_sets  <- n_total / n_reps

stopifnot(tasks == n_sets * n_reps)
set <- as.integer((index - 1) / n_reps) + 1
rep <- as.integer((index - 1) %% n_reps) + 1 

the_sim <- iw_obs_flat[set * n_reps + rep]
#pars_use  <- parameter_space[set * n_reps + rep, -ncol(parameter_space)]
message("processing set ", set, " rep: ", rep)

#seed_mle <- as.integer(Sys.time()) %% 1e6L * sample(1:10,1)
#set.seed(seed_mle)
#out <- iwABC::get_MLE(the_sim, pars_use = pars_use)


# # 5. parallel MLE over 100 replicates --------------------------------------
# #    split the_sim (length 100) across ncores
# res_list <- lapply(
#   seq_along(the_sim),
#   function(j) {
#     # before you call get_MLE, print which rep this is:
#     message(sprintf(
#       "[%s] Starting rep %3d on PID %d",
#       format(Sys.time(), "%H:%M:%S"),
#       j,
#       Sys.getpid()
#     ))
#     out <- iwABC::get_MLE(the_sim[[j]], pars_use = pars_use)
#     message(sprintf(
#       "[%s] Finished rep %3d",
#       format(Sys.time(), "%H:%M:%S"),
#       j
#     ))
#     out
#   }
# #  mc.cores            = ncores,
# #  mc.preschedule      = FALSE,      # better load balancing
# #  mc.allow.recursive  = FALSE
# )
# # `mc.preschedule = FALSE` hands out tasks one by one as workers free up, which
# #  improves load balancing at the cost of a bit more scheduling overhead.
# # `mc.allow.recursive = FALSE` forbids any further mclapply (or other forked calls)
# # inside your worker functions—safer if you know you won’t need nested parallelism.
# #

# # 5. aggregate & save ------------------------------------------------------
# res_df <- do.call(rbind, lapply(res_list, as.data.frame))
# outdir <- "~/iwABC/script/"
# if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
# outfile <- file.path(outdir, sprintf("debug_MLE_allpars_nomc_%02d.rds", i))

# saveRDS(res_df, file = outfile)
# message("Saved results to ", outfile)
