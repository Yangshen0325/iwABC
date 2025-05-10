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

# 1. parse args ------------------------------------------------------------
option_list <- list(
  make_option(c("--index"),  type="integer"),
  make_option(c("--outdir"))
)
opt <- parse_args(OptionParser(option_list=option_list))
index  <- opt$index
outdir <- opt$outdir
message(outdir)

# 2. load data -------------------------------------------------------------
parameter_space <- read.csv("~/iwABC/data/parameter_space_rep100_small_k.csv")
iw_obs_flat     <- readRDS("~/iwABC/data/iw_observations_rep100_small_k.rds")

n_total <- nrow(parameter_space)
n_reps  <- 100
stopifnot(index >= 1, index <= n_total)
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

sim <- as.integer((index - 1) / n_reps) + 1
rep <- as.integer((index - 1) %% n_reps) + 1
message(sim, " ", index, " ", rep)

the_sim <- iw_obs_flat[index]
pars_use <- parameter_space[index, -ncol(parameter_space)]

seed_mle <- as.integer(Sys.time()) %% 1e6L * sample(1:10,1)
set.seed(seed_mle)

# run single sim
out <- iwABC::get_MLE(the_sim[[1]], pars_use = pars_use)
out[["sim"]] <- sim;   # add sim index
out[["rep"]] <- rep;   # add rep
outfile <- file.path(outdir, sprintf("%d_%d.txt", i, j))
write.table(as.data.frame(out), file = outfile)

