out <- "~/iwABC/data/num_cycles5"

all_reps <- c();
for (sim in 1:48) {
  outdir <- file.path(out, sprintf("%.2d", sim))
  done <- list.files(outdir, pattern = ".txt", all.files = FALSE, recursive = FALSE)
  reps <- sapply(done, function(x) as.integer(substr(x, 1, 4)))
  reps <- (sim * 10000) + setdiff(1:1000, reps)
  reps <- sample(reps)
  nreps <- min(length(reps), 100 - length(done));
  if (nreps > 0) {
    all_reps <- c(all_reps, reps[1:nreps])
  }
  #cat(length(reps[1:min(length(reps), 100)]), " ", length(done), " ", max(0, nreps), "\n")
  cat(length(reps), " ", length(done), " ", max(0, nreps), "\n")
}
all_reps <- sample(all_reps)
s <- as.integer(all_reps / 10000)
r <- all_reps - (s * 10000)
#save(all_reps, file = "~/iwABC/script/run_list.Rdata")
