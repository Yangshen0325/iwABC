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


run_1 <- function(sim, rep, outdir, sim_list) {
  pivot <- sim_list[[sim]][[rep]]
  param <- pivot$param
  the_sim <- pivot$output
  seed_mle <- .Machine$integer.max - pivot$seed
  set.seed(seed_mle)
  outfile <- file.path(outdir, sprintf("%.4d.txt", rep))
  if (!file.exists(outfile)) {
    cat(outfile, "\n")
    out <- iwABC::get_MLE(the_sim[[1]], pars_use = param)
#    write.table(as.data.frame(out), file = outfile)
  }
}

# 1. parse args ------------------------------------------------------------
option_list <- list(
  make_option(c("--idx"),  type="integer"),    # array idx
  make_option(c("--ofs"),  type="integer"),
  make_option(c("--outdir"))
)
opt <- parse_args(OptionParser(option_list=option_list))

sim_list <- readRDS("~/iwABC/script/iw_sim_list.rds")
load("~/iwABC/script/run_list.Rdata")   # -> all_reps

idx <- opt$idx + opt$ofs
if (idx < length(all_reps)) {
  sr <- all_reps[idx]
  sim <- as.integer(sr / 10000)
  rep <- as.integer(sr - (sim * 10000))
  outdir <- file.path(opt$outdir, sprintf("%.2d", sim))
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  run_1(sim = sim, rep = rep, outdir = outdir, sim_list = sim_list)
}
