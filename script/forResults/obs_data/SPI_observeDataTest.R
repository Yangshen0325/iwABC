

# Present the observed data. Small-phylogeny islands

rm(list = ls())
library(tidyverse)
library(patchwork)
library(purrr)

# Read data
sim_list <- readRDS("script/forResults/mle_data/sim_list.rds")

#### test for one set ####
# set1 <- sim_list[[1]]
# the_set <- set1
# output_list <- lapply(set1, function(x) x$output[[1]])
# the_property <- "first_clade_size"
# # Check out the properties of each set, set1
# test <- lapply(output_list, calc_all_stats)
# output_df <- bind_rows(test, .id = "id")
# output_df$id <- factor(output_df$id, levels = 1:nrow(output_df))
# # Plot the `first_clade_size` scatter plot with `id` labeled on each dot.
# ggplot(output_df, aes(x = id, y = first_clade_size)) +
#   geom_point() +
#   labs(title = paste0(the_property, "of", the_set),
#        x = "Replicate ID",
#        y = "First Clade Size")
# ggplot(output_df, aes(y = first_clade_size)) +
#   geom_boxplot() +
#   labs(title = "First clade_size",
#        y = "First Clade Size")

#### function to check each parameter set ####
chk_each_param_set <- function(set_ID, property = "first_clade_size") {

    set_name <- paste0("set", set_ID)
    the_set <- sim_list[[set_ID]]

    output_list <- lapply(the_set, function(x) x$output[[1]])
    test <- lapply(output_list, calc_all_stats)
    output_df <- bind_rows(test, .id = "id")
    output_df$id <- factor(output_df$id, levels = 1:nrow(output_df))

    # Plot 1: Scatter plot
    p1 <- ggplot(output_df, aes(x = id, y = .data[[property]])) +
      geom_point() +
      labs(title = paste0(property, " of ", set_name),
           x = "Replicate ID",
           y = gsub("_", " ", property))

    # Plot 2: Boxplot
    p2 <- ggplot(output_df, aes(y = .data[[property]])) +
      geom_boxplot() +
      labs(title = paste("Distribution of", gsub("_", " ", property)),
           y = gsub("_", " ", property))

    # Plot 3: Histogram
    p3 <- ggplot(output_df, aes(x = .data[[property]])) +
      geom_histogram(
        bins = 15,
        fill = "lightgreen",     # fill color
        color = "black"         # border color
      ) +
      labs(title = property,
           x = property,
           y = "Count")

    # Plot 4: Number of species
    output_df$total_sp <- output_df$num_nonend_sim + output_df$num_sington_sim + output_df$num_multi_sim
    p4 <- ggplot(output_df, aes(x = total_sp)) +
      geom_histogram(
        bins = 15,
        fill = "steelblue",     # fill color
        color = "black"         # border color
      ) +
      labs(
        title = "Total number of species",
        x = "Total number of species",
        y = "Count"
      )

    # Print the plots
    print(p1)
    print(p2)
    print(p3)
    print(p4)
  }


# Property can be one of the following:
# "num_nonend_sim", "num_sington_sim", "num_multi_sim",
# "nonend_nltt", "singleton_nltt", "multi_nltt",
# "first_clade_size", "prop_largest_clade", "rank_largest_clade", "clade_evenness",
# "sim_ct_sd", "num_colon"
chk_each_param_set(set_ID = 4, property = "num_colon")

#### To check across parameter set ####
# Property can be one of the following:
# "num_nonend_sim", "num_sington_sim", "num_multi_sim",
# "nonend_nltt", "singleton_nltt", "multi_nltt",
# "first_clade_size", "prop_largest_clade", "rank_largest_clade", "clade_evenness",
# "sim_ct_sd", "num_colon"

set1 <- sim_list[[17]]
output_list <- lapply(set1, function(x) x$output[[1]])
test <- lapply(output_list, calc_all_stats)
test <- bind_rows(test)
p1 <- ggplot(test, aes(y = num_nonend_sim)) +
  geom_boxplot()


set2 <- sim_list[[17]]
output_list <- lapply(set2, function(x) x$output[[1]])
test <- lapply(output_list, calc_all_stats)
test <- bind_rows(test)
p2 <- ggplot(test, aes(y = num_sington_sim)) +
  geom_boxplot()
p1 + p2


set3 <- sim_list[[17]]
output_list <- lapply(set3, function(x) x$output[[1]])
test <- lapply(output_list, calc_all_stats)
test <- bind_rows(test)
p3 <- ggplot(test, aes(y = num_multi_sim)) +
  geom_boxplot()
p1 + p2 + p3

chk_across_set <- function(sim_list, property = "first_clade_size") {

  # Extract and compute stats for each set
  all_stats <- map_dfr(seq_along(sim_list), function(i) {
    output_list <- lapply(sim_list[[i]], function(x) x$output[[1]])
    stats_list <- lapply(output_list, calc_all_stats)
    df <- bind_rows(stats_list)
    df$param_set <- paste0("Set", i)
    return(df)
  })

  # Ensure param_set is a factor for ordered plotting
  all_stats$param_set <- factor(all_stats$param_set, levels = paste0("Set", seq_along(sim_list)))

  # Boxplot across parameter sets
  p <- ggplot(all_stats, aes(x = param_set, y = .data[[property]])) +
    geom_boxplot() +
    labs(title = paste("Distribution of", gsub("_", " ", property), "across parameter sets"),
         x = "Parameter Set",
         y = gsub("_", " ", property)) +
    theme_minimal()

  print(p)
}

chk_across_set(sim_list, property = "num_nonend_sim")
chk_across_set(sim_list, property = "num_sington_sim")
chk_across_set(sim_list, property = "num_multi_sim")
chk_across_set(sim_list, property = "nonend_nltt")
chk_across_set(sim_list, property = "singleton_nltt")
chk_across_set(sim_list, property = "multi_nltt")
chk_across_set(sim_list, property = "first_clade_size")
chk_across_set(sim_list, property = "prop_largest_clade")
chk_across_set(sim_list, property = "rank_largest_clade")
chk_across_set(sim_list, property = "clade_evenness")
chk_across_set(sim_list, property = "sim_ct_sd")
chk_across_set(sim_list, property = "num_colon")















##### check ######

reps <- 100
get_each_rep_res <- function(each_rep_res){

  summary_obs <- data.frame(total_sp = numeric(reps),
                            end_sp = numeric(reps),
                            nonend_sp = numeric(reps),
                            no_clades = numeric(reps),
                            largest_clade_size = numeric(reps),
                            first_clade_size = numeric(reps))
  for(i in 1:reps) {

    # Extract the stt_all matrix

    output <- each_rep_res[[i]][["output"]][[1]]
    if( !is.null(output)) {

    stt_all <- output[[1]][["stt_all"]]

    summary_obs$total_sp[i] <- sum(stt_all[nrow(stt_all), c("nI", "nA", "nC")])
    summary_obs$end_sp[i] <- sum(stt_all[nrow(stt_all), c("nA", "nC")])
    summary_obs$nonend_sp[i] <- sum(stt_all[nrow(stt_all), c("nI")])
    summary_obs$no_clades[i] <- length(output) - 1
    summary_obs$largest_clade_size[i] <- largest_clade_size(output)
    summary_obs$first_clade_size[i] <- first_clade_size(output)
    } else{
      summary_obs$total_sp[i] <- NA
      summary_obs$end_sp[i] <- NA
      summary_obs$nonend_sp[i] <- NA
      summary_obs$no_clades[i] <- NA
      summary_obs$largest_clade_size[i] <- NA
      summary_obs$first_clade_size[i] <- NA
    }
  }
  return(summary_obs)
}

obs_overview_spi <- lapply(sim_list, get_each_rep_res)
obs_overview_spi <- bind_rows(obs_overview_spi, .id = "column_label")

# combine with true values. 01 read data
parameter_space <- read_csv("data/parameter_space.csv")

obs_overview_spi$column_label <- factor(obs_overview_spi$column_label,
                                         levels = as.character(1:48))
# Pivot to long format for ggplot
obs_overview_spi_long <- obs_overview_spi %>%
  pivot_longer(-column_label, names_to = "metric", values_to = "value")

# Boxplot
ggplot(obs_overview_spi_long, aes(x = factor(column_label), y = value)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = "free_y") +
  theme_bw() +
  labs(x = "Parameter Set", y = "Value") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))









#### check the odd sets, reps ####
# check the odd set (those with extream estimates)




# 01 check the true values for the set

odd_set <- sim_list[[25]]
true_values <- lapply(odd_set, function(x) x$param) %>%
  bind_rows() %>%
  unique()
print(true_values)

# 02 check the odd island infomation results for the set
odd_island <- lapply(odd_set, function(x) x$output[[1]])

reps <- 100
summary_obs <- data.frame(total_sp = numeric(reps),
                          end_sp = numeric(reps),
                          nonend_sp = numeric(reps),
                          no_clades = numeric(reps),
                          largest_clade_size = numeric(reps),
                          first_clade_size = numeric(reps))
for(i in 1:reps) {
  # Extract the stt_all matrix
  stt_all <- odd_island[[i]][[1]][["stt_all"]]

  summary_obs$total_sp[i] <- sum(stt_all[nrow(stt_all), c("nI", "nA", "nC")])
  summary_obs$end_sp[i] <- sum(stt_all[nrow(stt_all), c("nA", "nC")])
  summary_obs$nonend_sp[i] <- sum(stt_all[nrow(stt_all), c("nI")])
  summary_obs$no_clades[i] <- length(odd_island[[i]]) - 1
  summary_obs$largest_clade_size[i] <- largest_clade_size(odd_island[[i]])
  summary_obs$first_clade_size[i] <- first_clade_size(odd_island[[i]])
}

summary_longer <- summary_obs %>%
  mutate(reps = row_number()) %>%
  pivot_longer(cols = -c(reps),
               names_to = "summary_type",
               values_to = "value")

# boxplot the odd island info
ggplot(summary_longer, aes( y = value)) +
  geom_boxplot() +
  facet_wrap(~ summary_type, scales = "free_y", ncol = 3) +
  labs(title = "Summary statistics for the odd set 25",
       y = "Value")


# 03 check the odd estimates for the set
# read data
odd_mle_df <- readRDS("script/forResults/mle_data/mle_df_data/mle_df_25.rds")

single_plot_mle <- odd_mle_df %>%
  mutate(reps = row_number())

ggplot(single_plot_mle, aes(x = reps, y = lac_MLE)) +
  geom_point(color = "lightgreen", size = 2) +
  geom_text(aes(label = reps), size = 4) +
  labs(title = "lac_MLE estimates for the odd set 25",
       x = "Replicate ID",
       y = "lac_MLE") +
  scale_y_continuous(limits = c(0, 70))

# delete the extreme outlier, check the distribution for the rest estimates
ggplot(single_plot_mle[-c(29), ], aes(x = reps, y = lac_MLE)) +
  geom_point(color = "lightgreen", size = 2)



# 04 check the phylogeny for the specific replicate
# can choose `total_sp`, `end_sp`, `nonend_sp`, `no_clades`, `largest_clade_size`, `first_clade_size`
single_plot_data <- summary_obs %>%
  mutate(reps = row_number())

phylo_info <- "first_clade_size"
ggplot(single_plot_data, aes(x = reps, y = first_clade_size)) +
  geom_point(color = "steelblue", size = 1) +
  labs(title = paste0(phylo_info, " for the odd set 25"),
       x = "Replicate ID",
       y = phylo_info) +
  geom_text(aes(label = reps), size = 3)

# make it a function
plot_phylo <- function(plot_dat, phylo_info) {

  ggplot(plot_dat, aes(x = reps, y = .data[[phylo_info]])) +
    geom_point(color = "steelblue", size = 1) +
    labs(title = paste0(phylo_info),
         x = "Replicate ID",
         y = phylo_info) +
    geom_text(aes(label = reps), size = 3)
}
p1 <- plot_phylo(single_plot_data, "total_sp")
p2 <- plot_phylo(single_plot_data, "end_sp")
p3 <- plot_phylo(single_plot_data, "nonend_sp")
p4 <- plot_phylo(single_plot_data, "no_clades")
p5 <- plot_phylo(single_plot_data, "largest_clade_size")
p6 <- plot_phylo(single_plot_data, "first_clade_size")

p1 + p2 + p3 + p4 + p5 + p6
p1 + p4
p1 + p2
# Or check the data for the specific rep
single_plot_data[29, ] # rep 48 is odd
mle_df_25[29,]



