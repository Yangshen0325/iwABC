# the output folder
out <- "~/iwABC/data/num_cycles5"

todo_reps <- c();
for (sim in 1:48) {
  outdir <- file.path(out, sprintf("%.2d", sim))
  done <- list.files(outdir, pattern = ".txt", all.files = FALSE, recursive = FALSE)
  reps <- sapply(done, function(x) as.integer(substr(x, 1, 4)))
  reps <- (sim * 10000) + setdiff(1:1000, reps)
  reps <- sample(reps)
  nreps <- min(length(reps), 100 - length(done));
  if (nreps > 0) {
    todo_reps <- c(todo_reps, reps[1:nreps])
  }
}
todo_reps <- sample(todo_reps)
cat("to do: ", length(todo_reps), "\n")
save(todo_reps, file = "~/iwABC/script/run_list.Rdata")
