
# Check out the distribution of differences of summary statistics, then define epsilon


# function to get ss_differ -----------------------------------------------

# return `ss_diff_list` for checking the distribution of summary statistics difference,
# defining the epsilon

t_ABC_SMC_ss_distri <- function(
    obs_data,
    calc_ss_function = calc_error_all,
    #init_epsilon_values = init_epsilon,
    prior_generating_function = prior_gen,
    prior_density_function = prior_dens,
    number_of_particles = number_of_particles,
    sigma = 0.05,
    stop_rate = 1e-3,
    num_iterations = num_iterations,
    idparsopt = c(1, 2, 3, 4, 5),
    pars = parameters
    #ss_set
) {

  obs_data <- obs_sim
  #generate a matrix with epsilon values
  #we assume that the SMC algorithm converges within 50 iterations
  #epsilon <- matrix(nrow = 20, ncol = length(init_epsilon_values))
  #epsilon[1, ] <- init_epsilon_values

  #store weights
  new_weights <- c()
  new_params <- list(c(seq_along(pars)))
  previous_weights <- c()
  previous_params  <- list(c(seq_along(pars)))
  indices <- 1:number_of_particles
  n_iter <- 0
  ABC_list <- list()
  sim_list <- list()
  ss_diff_list <- list()

  # mainland species pool
  M <- 1000

  #convergence is expected within 50 iterations
  #usually convergence occurs within 20 iterations
  for (i in 1:num_iterations) {
    ss_diff <- c()
    n_iter <- n_iter + 1
    cat("\nGenerating Particles for iteration\t", i, "\n")
    cat("0--------25--------50--------75--------100\n")
    cat("*")
    utils::flush.console()

    print_frequency <- 20
    tried <- 0
    number_accepted <- 0
    sigma_temp <- sigma * exp(-0.2 * (i - 1))

    #replace all vectors
    if (i > 1) {
      #normalize the weights and store them as previous weights.
      previous_weights <- new_weights / sum(new_weights)
      new_weights <- c() #remove all currently stored weights
      previous_params <- new_params #store found params
      new_params <- list(c(seq_along(parameters))) #clear new params
    }

    stoprate_reached <- FALSE
    # ss_logic <- c()

    while (number_accepted < number_of_particles) {
      #in this initial step, generate parameters from the prior
      if (i == 1) {
        parameters <- prior_generating_function(pars, idparsopt)
      } else {
        #if not in the initial step, generate parameters
        #from the weighted previous distribution:
        index <- sample(x = indices, size = 1,
                        replace = TRUE, prob = previous_weights)

        for (p_index in seq_along(parameters)) {
          parameters[p_index] <- previous_params[[index]][p_index]
        }
        parameters[idparsopt] <- exp(log(parameters[idparsopt]) +
                                       stats::rnorm(length(idparsopt),
                                                    0, sigma_temp))
      }

      #reject if outside the prior
      if (prior_density_function(pars, idparsopt) > 0) {

        #simulate a new tree, given the proposed parameters
        new_sim <- DAISIE::DAISIE_sim_cr(
          time = 5,
          M = M,
          pars = as.numeric(c(parameters[1], parameters[2], parameters[3], parameters[4], parameters[5])),
          replicates = 1,
          divdepmodel = "IW",
          nonoceanic_pars = c(0, 0),
          sample_freq  = Inf,
          plot_sims = FALSE,
          verbose = FALSE,
          cond = 1
        )

        accept <- TRUE

        #calculate the summary statistics for the simulated tree
        if (accept) {
          df_stats <- calc_ss_function(sim_1 = obs_data,
                                       sim_2 = new_sim[[1]])

        }

        if (accept) {
          number_accepted <- number_accepted + 1
          new_params[[number_accepted]] <- parameters
          sim_list[[number_accepted]] <- new_sim[[1]]
          accepted_weight <- 1
          ss_diff <- rbind(ss_diff,df_stats)

          #calculate the weight
          if (i > 1) {
            accepted_weight <- calc_weight(previous_weights,
                                           previous_params,
                                           parameters,
                                           sigma_temp,
                                           prior_density_function,
                                           idparsopt)
          }

          new_weights[number_accepted] <- accepted_weight

          if ((number_accepted) %%
              (number_of_particles / print_frequency) == 0) {
            cat("**")
            utils::flush.console()
          }
        }
      }

      #convergence if the acceptance rate gets too low
      tried <- tried + 1
      if (tried > (1 / stop_rate) & n_iter > 4) {

        if ((number_accepted / tried) < stop_rate) {
          stoprate_reached <- TRUE
          break
        }
      }
    }

    ss_diff_list[[i]] <- ss_diff

    # if (stoprate_reached == FALSE) {
    #   epsilon[i + 1, ] <- apply(ss_diff, 2, quantile, probs = 0.5)
    # }

    ABC <- c()
    for (k in seq_along(new_params)) {
      add <- c()
      for (m in seq_along(parameters)) {
        add <- c(add, new_params[[k]][m])
      }
      ABC <- rbind(ABC, add)
    }
    ABC_list[[i]] <- ABC

    if (stoprate_reached) {
      break
    }
  }
  message("tried times: ", tried)

  output <- list(sim_list = sim_list,
                 ABC = ABC_list,
                 n_iter = n_iter,
                 #epsilon = epsilon,
                 obs_sim = obs_data,
                 ss_diff_list = ss_diff_list)
  return(output)
}




# Distribution of difference across different combinations of para --------
rm(list = ls())
# read empirical data
iw_observations <- readRDS("script/iw_observations.rds")

# read parameters generating empirical data
parameter_space <- read.csv("script/parameter_space.csv")

# number if iteration
num_iterations <- 1

# number of particles
number_of_particles <- 500

M <- 1000

# initialize the results
t_ss_distri <- list()

# Get the returns across all scenarios
for (i in seq_along(iw_observations)){

  obs_sim <- iw_observations[[i]]
  parameters <- as.numeric(parameter_space[i, c(1:5)])

  # calculate the differences of summary statistics
  t_ss_distri[[i]] <- t_ABC_SMC_ss_distri(obs_data = obs_sim,
                                     calc_ss_function <- calc_error_all,
                                     #init_epsilon_values = init_epsilon,
                                     prior_generating_function <- prior_gen,
                                     prior_density_function <- prior_dens,
                                     number_of_particles = number_of_particles,
                                     sigma = 0.05,
                                     stop_rate = 1e-3,
                                     num_iterations = num_iterations,
                                     idparsopt = c(1, 2, 3, 4, 5),
                                     pars = parameters)

}

saveRDS(t_ss_distri, file = "small_test/define_epsilon/t_ss_distri.rds")




# Visualize ---------------------------------------------------------------

# read data
t_ss_distri <- readRDS("~/Downloads/phd_yang/pkgs/iwABC/small_test/define_epsilon/t_ss_distri.rds")
# format data
ss_diff_list <- lapply(t_ss_distri, function(x) x$ss_diff_list[[1]])

 # set column names
column_names <- c(
  "diff_total_nltt", "diff_singleton_nltt", "diff_multi_nltt", "diff_nonend_nltt",
  "diff_colon_time", "diff_clade_size",
  "diff_total", "diff_sington", "diff_multi", "diff_noend",
  "diff_colon",
  "diff_largest_cs", "diff_first_cs",
  "diff_prop_largest_clade"
)
ss_diff_table <- lapply(ss_diff_list, function(mat) {
  colnames(mat) <- column_names
  return(mat)
})

 # combine data across scenarios
combined_data <- do.call(rbind, lapply(seq_along(ss_diff_table), function(i) {
  data <- as.data.frame(ss_diff_table[[i]])
  data$Scenario <- i  # Add a column for the scenario number
  return(data)
}))





# Plotting
library(tidyverse)

# longer data
long_data <- combined_data  |>
  pivot_longer(
    cols = -Scenario,
    names_to = "Statistic",
    values_to = "Difference"
  )

# Define plot functions
plot_box <- function(data, title) {
  ggplot(data, aes(x = as.factor(Scenario), y = Difference, fill = as.factor(Scenario))) +
    geom_boxplot(outlier.alpha = 0.3) +
    facet_wrap(~Statistic, scales = "free", ncol = 1) +
    labs(
      title = title,
      x = "Scenario",
      y = "Difference",
      fill = "Scenario"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}


# group summary statistics for plotting
groups <- list(
  group1 = c("diff_total_nltt", "diff_singleton_nltt", "diff_multi_nltt", "diff_nonend_nltt"),
  group2 = c("diff_colon_time", "diff_clade_size", "diff_colon"),
  group3 = c("diff_total", "diff_songton", "diff_multi", "diff_noend"),
  group4 = c("diff_largest_cs", "diff_first_cs", "diff_prop_largest_clade")

)

# box plot
for (group_name in names(groups)) {

  group <- groups[[group_name]]

  # Filter data for the group
  group_data <- long_data  |>
    filter(Statistic %in% group)

  # Create box plot
  box_plot <- plot_box(group_data, paste("Box Plot for", group_name))
  print(box_plot)

}


# calculate the median and 95th values
summary_stats <- long_data  |>
  group_by(Statistic)  |>
  summarize(
    median = median(Difference),
    Percentile_95 = quantile(Difference, probs = 0.95),
    max_val = max(Difference)
  )

print(summary_stats)

# A tibble: 14 × 4
# Statistic               median Percentile_95 max_val
# <chr>                    <dbl>         <dbl>   <dbl>
#   1 diff_clade_size          1.13          3.63   24.6
# 2 diff_colon               8            24      47
# 3 diff_colon_time          0.299         1.18    2.43
# 4 diff_first_cs            2            10      46
# 5 diff_largest_cs          4            13      45
# 6 diff_multi              14            56      82
# 7 diff_multi_nltt         34.0         133.    265.
# 8 diff_noend               3             9      45
# 9 diff_nonend_nltt         4.53         22.4   118.
# 10 diff_prop_largest_clade  0.124         0.538   0.898
# 11 diff_singleton_nltt     12.7          47.1   152.
# 12 diff_sington             4            16      45
# 13 diff_total              17            59      91
# 14 diff_total_nltt         42.0         145.    293.

# If you want to plot density plot (too many lines, not recommended) --------


plot_density <- function(data, title) {
  ggplot(data, aes(x = Difference, fill = as.factor(Scenario), color = as.factor(Scenario))) +
    geom_density(alpha = 0.3) +
    facet_wrap(~Statistic, scales = "free", ncol = 1) +
    labs(
      title = title,
      x = "Difference",
      y = "Density",
      fill = "Scenario",
      color = "Scenario"
    ) +
    theme_minimal()
}

# box plot
for (group_name in names(groups)) {

  group <- groups[[group_name]]

  # filter data
  group_data <- long_data  |>
    filter(Statistic %in% group)

  density_plot <- plot_density(group_data, paste("Density Plot for", group_name))
  print(density_plot)

}


print(density_plot)










