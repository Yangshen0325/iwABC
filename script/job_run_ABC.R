args <- commandArgs(TRUE)

# Uncomment for local testing:
# terminal_command <- c(1, 2, 3, 4, 5, 0)
# args <- c(1, terminal_command)


param_set <- as.numeric(args[1])
idparsopt_lac <- as.numeric(args[2])
idparsopt_mu <- as.numeric(args[3])
idparsopt_K <- as.numeric(args[4])
idparsopt_gam <- as.numeric(args[5])
idparsopt_laa <- as.numeric(args[6])
ss_set <- 0 # as.numeric(args[7])

idparsopt_all <- c(idparsopt_lac, idparsopt_mu, idparsopt_K, idparsopt_gam, idparsopt_laa)

idparsopt <- idparsopt_all #which(idparsopt_all == 1)

saveOrNot <- TRUE

metadata <- paste0("This is parameter set ", param_set)



library(iwABC)

t0 <- Sys.time()

res <- iwABC::run_ABC_par(
  param_set = as.numeric(args[1]),
  idparsopt = as.numeric(idparsopt),
  saveOrNot = saveOrNot,
  ss_set = 0,
  number_of_particles = 100,
  num_iterations = 8,
  num_threads = 10,
  stop_rate = 1e-7,
  start_of_file_name = paste0("set_", param_set, "_")
)

t1 <- Sys.time()

difftime(t1, t0)

want_to_plot <- FALSE

if (want_to_plot) {


require(tidyverse)

res <- readRDS("/Users/thijsjanzen/Documents/GitHub/iwABC/results/param_set_1_ss_0.rds")

to_plot <- c()
for (i in 1:length(res$ABC)) {
 to_add <- cbind(i, res$ABC[[i]])
 to_plot <- rbind(to_plot, to_add)
}

colnames(to_plot) <- c("iteration", "lac", "mu", "K", "gam", "laa")
to_plot <- as_tibble(to_plot)
to_plot %>%
  gather(key = parameter, value = "value", -iteration) %>%
  ggplot(aes(x = iteration, y = value, group = iteration)) +
    geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~parameter, scales = "free")
}
