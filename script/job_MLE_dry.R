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


run_n <- function(n, sim, ofs, outdir, sim_list) {
  done <- list.files(outdir, pattern = ".txt", all.files = FALSE, recursive = FALSE)
  if (length(done) > 100) {
    return()
  }
  reps <- sim_list[[sim]]
  for (i in 1:n) {
    idx <- i + ofs
    rep <- reps[[idx]]
    param <- rep$param
    the_sim <- rep$output
    seed_mle <- .Machine$integer.max - rep$seed
    set.seed(seed_mle)
    outfile <- file.path(outdir, sprintf("%.4d.txt", idx))
    if (!file.exists(outfile)) {
      out <- iwABC::get_MLE(the_sim[[1]], pars_use = param)
      write.table(as.data.frame(out), file = outfile)
    }
  }
}

# 1. parse args ------------------------------------------------------------
option_list <- list(
  make_option(c("--sim"),  type="integer"),
  make_option(c("--ofs"),  type="integer"),
  make_option(c("--outdir"))
)
opt <- parse_args(OptionParser(option_list=option_list))
sim <- opt$sim
ofs <- opt$ofs
outdir <- file.path(opt$outdir, sprintf("%.2d", sim))
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# load data
sim_list <- readRDS("~/iwABC/script/iw_sim_list.rds")
run_n(n = 100, sim = sim, ofs = ofs, outdir = outdir, sim_list = sim_list)
