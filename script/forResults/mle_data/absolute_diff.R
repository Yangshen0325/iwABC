
library(tidyverse)
library(GGally)

#### MLE results. 5 parameters have 10 pair combinations. Plot the difference
#### with true values. 4800 replications. (4800 points for each kind, for now,
#### only MLE results)

# read data
mle_true_bias <- readRDS("script/forResults/mle_data/mle_true_bias.rds")

# PLot 10 pairwise scatter plots
bias_df <- mle_true_bias %>%
  select(bias_lac, bias_mu, bias_K, bias_gam, bias_laa)

# plot using ggpairs, show only lower triangle
pairs(bias_df)

ggpairs(
  bias_df,
  lower = list(continuous = wrap("points", alpha = 0.5, size = 1)),
  diag = list(continuous = "blankDiag"),  # remove histograms
  upper = list(continuous = "blank")      # blank upper triangle
) +
  theme_minimal(base_size = 12)



# Plot bias data for each parameters ---------------------------------------------------------

## gam, immigration rate
# set a cutoff value, make them Inf and then report the proportion of Inf
cutoff <- 1
gam_mle_true_bias <- mle_true_bias %>%
  select(param_set, bias_gam, gam, gam_MLE) %>%
  mutate(
    is_extreme = bias_gam > cutoff * gam | !is.finite(gam_MLE),
    bias_censored = ifelse(is_extreme, Inf, bias_gam)
  )
gam_finite <- gam_mle_true_bias %>%
  filter(is.finite(bias_censored))
gam_inf <- gam_mle_true_bias %>%
  group_by(param_set) %>%
  summarise(inf_ratio = mean(is_extreme), .groups = "drop")

ggplot(gam_finite, aes(x = factor(param_set), y = bias_censored)) +
  geom_boxplot() +
  # Add points for true values
  geom_point(data = gam_finite,
             aes(factor(param_set), y = gam),
             color = "blue", size = 2, shape = 16) +
  # Add % Inf annotations
  geom_text(data = gam_inf,
            aes(x = factor(param_set),
                y = 1.5 * max(gam_finite$bias_censored, na.rm = TRUE),
                label = paste0("Inf: ", round(100 * inf_ratio), "%")),
            size = 3,
            color = "red") +
  labs(x = "Parameter Set", y = "Bias (|MLE - True|)", title = "Gam") +
  theme_minimal(base_size = 14)



# Function!
plot_bias_box_with_inf <- function(data, param, cutoff = 100) {

  bias_col <- paste0("bias_", param)
  true_col <- param
  mle_col <- paste0(param, "_MLE")

  # Prepare the data
  df <- data %>%
    select(param_set, !!sym(bias_col), !!sym(true_col), !!sym(mle_col)) %>%
    rename(
      bias = !!sym(bias_col),
      true_value = !!sym(true_col),
      estimate = !!sym(mle_col)
    ) %>%
    mutate(
      is_extreme = bias > cutoff * true_value | !is.finite(estimate),
      bias_censored = ifelse(is_extreme, Inf, bias)
    )

  df_finite <- df %>%
    filter(is.finite(bias_censored))

  df_inf <- df %>%
    group_by(param_set) %>%
    summarise(inf_ratio = mean(is_extreme), .groups = "drop")

  # Plot
  ggplot(df_finite, aes(x = factor(param_set), y = bias_censored)) +
    geom_boxplot() +
    # True value as blue point
    geom_point(data = df_finite,
               aes(x = factor(param_set), y = true_value),
               color = "blue", size = 2, shape = 16) +
    # % Inf annotation
    geom_text(data = df_inf,
              aes(x = factor(param_set),
                  y = 1.5 * max(df_finite$bias_censored, na.rm = TRUE),
                  label = paste0("Inf: ", round(100 * inf_ratio), "%")),
              size = 3,
              color = "red") +
    labs(
      x = "Parameter Set",
      y = "Bias (|MLE - True|)",
      title = paste0("Bias in ", param, " Estimates")
    ) +
    theme_minimal(base_size = 14)
}

plot_bias_box_with_inf(mle_true_bias, param = "gam", cutoff = 1)
plot_bias_box_with_inf(mle_true_bias, param = "lac", cutoff = 1)
plot_bias_box_with_inf(mle_true_bias, param = "K", cutoff = 1)
plot_bias_box_with_inf(mle_true_bias, param = "laa", cutoff = 1)

# true values of mu contain 0, so a fixed value, check


plot_bias_box_with_inf <- function(data, param, cutoff = 100, abs_cutoff = 1e-3) {

  bias_col <- paste0("bias_", param)
  true_col <- param
  mle_col <- paste0(param, "_MLE")

  df <- data %>%
    select(param_set, !!sym(bias_col), !!sym(true_col), !!sym(mle_col)) %>%
    rename(
      bias = !!sym(bias_col),
      true_value = !!sym(true_col),
      estimate = !!sym(mle_col)
    ) %>%
    mutate(
      is_extreme = case_when(
        is.na(bias) | !is.finite(estimate) ~ TRUE,
        true_value == 0 ~ bias > abs_cutoff,
        TRUE ~ bias > cutoff * true_value
      ),
      bias_censored = ifelse(is_extreme, Inf, bias)
    )

  df_finite <- df %>% filter(is.finite(bias_censored))

  df_inf <- df %>%
    group_by(param_set) %>%
    summarise(inf_ratio = mean(is_extreme), .groups = "drop")

  ggplot(df_finite, aes(x = factor(param_set), y = bias_censored)) +
    geom_boxplot() +
    # Add true value as blue point
    geom_point(data = df_finite,
               aes(x = factor(param_set), y = true_value),
               color = "blue", size = 2, shape = 16) +
    # Add % Inf annotation
    geom_text(data = df_inf,
              aes(x = factor(param_set),
                  y = 1.5 * max(df_finite$bias_censored, na.rm = TRUE),
                  label = paste0("Inf: ", round(100 * inf_ratio), "%")),
              size = 3,
              color = "red") +
    labs(
      x = "Parameter Set",
      y = "Bias (|MLE - True|)",
      title = paste0("Bias in ", param, " Estimates")
    ) +
    theme_minimal(base_size = 14)
}

plot_bias_box_with_inf(mle_true_bias, "mu", cutoff = 100, abs_cutoff = 1)







# suggest new: K = K / (K + K_true)
# mle_list_df <- mle_list_df %>%
#   mutate(K_MLE = ifelse(is.infinite(K_MLE), 1, K_MLE / (K_MLE + 1)),
#          param_set = as.integer(param_set))


############################.
# incase we want to know how many Inf of K in each set
# all_mle_tb <- bind_rows(mle_list, .id = "param_set")
#
# all_mle_tb %>%
#   mutate(is_inf = ifelse(is.infinite(K_MLE), 1, 0)) %>%
#   select(is_inf, param_set) %>%
#   group_by(param_set) %>%
#   summarise(n_inf = sum(is_inf))
# A tibble: 5 × 2
# param_set n_inf
# <chr>     <dbl>
# 1 08            4
# 2 13            2
# 3 14            2
# 4 26            3
# 5 38            1


#############################.

geom_rug


################ weird bias with lac, and laa???? why? check out the distribution of
#############################.
#### 5 estimated values， boxplot, across 7 sets of parameters

mle_long <- mle_list_df %>%
  select(-max_ll) %>%
  pivot_longer(cols = -param_set,
               names_to = "parameters",
               values_to = "estimate")
ggplot(mle_long, aes(x = factor(param_set), y = estimate)) +
  geom_boxplot() +
  facet_wrap(~ parameters, scales = "free_y", ncol = 3)

# the third set of parameters has a very high lac, and gam,
# read island info of the third set


# Extract the third set of mle results
third_mle <- mle_list[["03"]]
single_plot_mle <- third_mle %>%
  mutate(reps = row_number())

ggplot(single_plot_mle, aes(x = reps, y = lac_MLE)) +
  geom_point(color = "steelblue", size = 2) +
  geom_text(aes(label = reps), size = 3)


sim_list <- readRDS("~/Downloads/phd_yang/pkgs/iwABC/script/forResults/mle_data/sim_list.rds")

third_set <- sim_list[[3]]
true_values <- lapply(third_set, function(x) x$param) %>%
  bind_rows() %>%
  unique()
print(true_values)
# > print(true_values)
# lac  mu  K   gam laa
# 1 0.4 0.3 20 0.001 0.1
third_island <- lapply(third_set, function(x) x$output[[1]])

reps <- 100
summary_obs <- data.frame(total_sp = numeric(reps),
                          end_sp = numeric(reps),
                          nonend_sp = numeric(reps),
                          no_clades = numeric(reps),
                          largest_clade_size = numeric(reps),
                          first_clade_size = numeric(reps))
for(i in 1:reps) {
  # Extract the stt_all matrix
  stt_all <- third_island[[i]][[1]][["stt_all"]]

  summary_obs$total_sp[i] <- sum(stt_all[nrow(stt_all), c("nI", "nA", "nC")])
  summary_obs$end_sp[i] <- sum(stt_all[nrow(stt_all), c("nA", "nC")])
  summary_obs$nonend_sp[i] <- sum(stt_all[nrow(stt_all), c("nI")])
  summary_obs$no_clades[i] <- length(third_island[[i]]) - 1
  summary_obs$largest_clade_size[i] <- largest_clade_size(third_island[[i]])
  summary_obs$first_clade_size[i] <- first_clade_size(third_island[[i]])
}

summary_longer <- summary_obs %>%
  mutate(reps = row_number()) %>%
  pivot_longer(cols = -c(reps),
               names_to = "summary_type",
               values_to = "value")

ggplot(summary_longer, aes( y = value)) +
  geom_boxplot() +
  facet_wrap(~ summary_type, scales = "free_y", ncol = 3)


single_plot_data <- summary_obs %>%
  mutate(reps = row_number())

ggplot(single_plot_data, aes(x = reps, y = end_sp)) +
  geom_point(color = "steelblue", size = 2) +
  geom_text(aes(label = reps), size = 3)





#############################.
#############################.
