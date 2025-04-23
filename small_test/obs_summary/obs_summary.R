
#### Try different combinations of parameters to get arounf 10 clades


rm(list = ls())

iw_DAISIE_sim <- function(parameters) {
  success <- FALSE  # Initialize a flag to check if the simulation succeeded

  while (!success) {  # Keep trying until the simulation succeeds
    tryCatch({
      # Attempt to run the simulation
      iw_sim <- DAISIE_sim_cr(
        time = 5,
        M = 1000,
        pars = as.numeric(c(parameters[1], parameters[2], parameters[3], parameters[4], parameters[5])),
        replicates = 1,
        divdepmodel = "IW",
        nonoceanic_pars = c(0, 0),
        sample_freq  = Inf,
        plot_sims = FALSE,
        verbose = FALSE,
        cond = 1
      )

      # If no error, set success to TRUE and exit the loop
      success <- TRUE
      message("Simulation completed successfully.")

    }, error = function(e) {
      # If an error occurs, display a message and retry
      message("Error encountered: ", e$message)
      message("Retrying the simulation...")
      # The loop will continue automatically as success is still FALSE
    })
  }

  return(iw_sim)  # Return the successful simulation result
}





## Define the parameters
lac <- c(0.4, 0.7)
mu <- c(0, 0.3)
K <- c(20, 100)
gam <- c(0.001, 0.002)
laa <- c(0.1, 1.0)

## Define replicates
rep <- 10

## Create all combinations
param_space <- expand.grid(lac = lac,
                           mu = mu,
                           K = K,
                           gam = gam,
                           laa = laa
)

## Replicate each row of the parameter space 10 times
rep_param_space <- param_space[rep(seq_len(nrow(param_space)), each = rep), ]

## Add a column for the replicate number (1 to 10)
rep_param_space$rep <- rep(1:rep, times = nrow(param_space))

parameter_space <- rep_param_space


# # Initialize the space for outputs
iw_sim_list <- list()
#
# # Iterate through each row of the parameter space
for (i in seq_len(nrow(parameter_space))) {
  cat("rep is ", i, "\n")
  # Extract the parameter set
  parameters <-parameter_space[i, c("lac", "mu", "K", "gam", "laa")]

  # Show the exact parameter
  cat("Running simulation for parameter set:\n")
  cat("lac=", parameters$lac, ", mu=", parameters$mu, "K=", parameters$K, "gam=",
      parameters$gam, "laa=", parameters$laa)

  # Run the simulation with the current parameter set
  iw_sim_list[[i]] <- iw_DAISIE_sim(parameters = parameters)

}
#
iw_observations <- lapply(iw_sim_list, function(x) x[[1]])



summary_obs <- data.frame(total_sp = numeric(length(iw_observations)),
                          end_sp = numeric(length(iw_observations)),
                          nonend_sp = numeric(length(iw_observations)),
                          no_clades = numeric(length(iw_observations)),
                          largest_clade_size = numeric(length(iw_observations)),
                          first_clade_size = numeric(length(iw_observations)))

for (i in seq_along(iw_observations)) {
  # Extract the stt_all matrix
  stt_all <- iw_observations[[i]][[1]][["stt_all"]]

  summary_obs$total_sp[i] <- sum(stt_all[nrow(stt_all), c("nI", "nA", "nC")])
  summary_obs$end_sp[i] <- sum(stt_all[nrow(stt_all), c("nA", "nC")])
  summary_obs$nonend_sp[i] <- sum(stt_all[nrow(stt_all), c("nI")])
  summary_obs$no_clades[i] <- length(iw_observations[[i]]) - 1
  summary_obs$largest_clade_size[i] <- largest_clade_size(iw_observations[[i]])
  summary_obs$first_clade_size[i] <- first_clade_size(iw_observations[[i]])

}
param_data <- parameter_space

obs_combined <- bind_cols(param_data %>% select(-rep), summary_obs)


obs_combined <- obs_combined %>%
  mutate(scenario = rep(1:32, each = 10))

# Pivot to long format for ggplot
obs_long <- obs_combined %>%
  select(scenario, total_sp:first_clade_size) %>%
  pivot_longer(-scenario, names_to = "metric", values_to = "value")

# Boxplot
p <- ggplot(obs_long, aes(x = factor(scenario), y = value)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = "free_y") +
  theme_bw() +
  labs(x = "Scenario", y = "Value",
       title = 'lac=0.4_0.7, mu=0_0.3,
       K=20_100, gam=0.001_0.002, laa=.1_1.0') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

ggsave(filename = "small_test/obs_summary/p1.png",
       plot = p,
       width = 10,
       height = 6,
       dpi = 300,
       units = "in",
       device = "png")
