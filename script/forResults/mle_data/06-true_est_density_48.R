library(dplyr)
library(ggplot2)
# read the data with param_set, true values, estimates, absolute bias
mle_true_bias <- readRDS("script/forResults/mle_data/mle_true_bias.rds")
# 1. Summaries per param_set: median MLE and true value
cutoff <- 7
lac_noodd <- mle_true_bias %>%
  select(param_set, lac_MLE, lac, bias_lac) %>%
  mutate(
    is_extreme = bias_lac > cutoff * lac | !is.finite(lac_MLE), # if the estimates over 6 times of true
    lac_MLE_plot = ifelse(is_extreme, NA_real_, lac_MLE)  #
  )

lac_summary <- lac_noodd %>%
  group_by(param_set) %>%
  summarise(
    inf_ratio = mean(is_extreme),
    median_lac_MLE = median(lac_MLE_plot, na.rm = TRUE),
    true_lac       = first(lac),  # lac is constant within a param_set
    .groups = "drop"
  )

ggplot(lac_summary, aes(param_set, y = inf_ratio)) +
  geom_bar(stat = "identity", fill = "#d6cdb8", color = NA) +
  # make y-axis in percentage
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Parameter Set",
    y = "Proportion of extreme estimates"
  ) +
  theme_bw(base_size = 12)

# if don't exclude odd values
lac_summary_all <- mle_true_bias %>%
  group_by(param_set) %>%
  summarise(
    median_lac_MLE = median(lac_MLE),
    true_lac       = first(lac),
    .groups = "drop"
  )

ggplot(mle_true_bias, aes(x = lac_MLE)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "#d6cdb8",
                 na.rm = TRUE,
                 color = NA) +
  geom_vline(data = lac_summary_all,
             aes(xintercept = median_lac_MLE),
             linewidth = 0.6,
             na.rm = TRUE,
             color = "black") +
  geom_vline(data = lac_summary_all,
             aes(xintercept = true_lac),
             linewidth = 0.6,
             na.rm = TRUE,
             linetype = "dashed",
             color = "steelblue") +
  facet_wrap(~ param_set, scales = "free_x", ncol = 6) +
  labs(
    x = expression(lambda[c]^" (MLE)"),
    y = "Density",
    title = expression("Distribution of " * lambda[c] * " MLE across 48 parameter settings")
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(0.8, "lines")
  )


# 2. Faceted histogram / density-style plot across 48 settings
lac_48 <- ggplot(lac_noodd, aes(x = lac_MLE_plot)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "#d6cdb8",
                 color = NA,
                 na.rm = TRUE) +
  # median of MLEs (black)
  geom_vline(data = lac_summary,
             aes(xintercept = median_lac_MLE),
             linewidth = 0.6,
             #linetype = "dashed",
             color = "black") +
  # true value (blue, dashed)
  geom_vline(data = lac_summary,
             aes(xintercept = true_lac),
             linewidth = 0.6,
             linetype = "dashed",
             color = "steelblue") +
  facet_wrap(~ param_set, scales = "free_x", ncol =6) +
  labs(
    x = expression(lambda^c),
    y = "Density"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )



# mu ----------------------------------------------------------------------

cutoff <- 1.0
mu_noodd <- mle_true_bias %>%
  select(param_set, mu_MLE, mu, bias_mu) %>%
  mutate(
    is_extreme = bias_mu > cutoff | !is.finite(mu_MLE),
    mu_MLE_plot = ifelse(is_extreme, NA_real_, mu_MLE)  #
  )

mu_summary <- mu_noodd %>%
  group_by(param_set) %>%
  summarise(
    inf_ratio = mean(is_extreme),
    median_mu_MLE = median(mu_MLE_plot, na.rm = TRUE),
    true_mu       = first(mu),  # mu is constant within a param_set
    .groups = "drop"
  )

ggplot(mu_summary, aes(param_set, y = inf_ratio)) +
  geom_bar(stat = "identity", fill = "#d6cdb8", color = NA) +
  # make y-axis in percentage
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Parameter Set",
    y = "Proportion of extreme estimates"
  ) +
  theme_bw(base_size = 12)

# 2. Faceted histogram / density-style plot across 48 settings

ggplot(mu_noodd, aes(x = mu_MLE_plot)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "#d6cdb8",
                 color = NA,
                 na.rm = TRUE) +
  # median of MLEs (black)
  geom_vline(data = mu_summary,
             aes(xintercept = median_mu_MLE),
             linewidth = 0.6,
             #linetype = "dashed",
             color = "black") +
  # true value (blue, dashed)
  geom_vline(data = mu_summary,
             aes(xintercept = true_mu),
             linewidth = 0.6,
             linetype = "dashed",
             color = "steelblue") +
  facet_wrap(~ param_set, scales = "free_x", ncol = 6) +
  labs(
    x = expression(mu),
    y = "Density"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )


# K -----------------------------------------------------------------------

cutoff <- 1.5
K_noodd <- mle_true_bias %>%
  select(param_set, K_MLE, K, bias_K) %>%
  mutate(
    is_extreme = bias_K > cutoff * K | !is.finite(K_MLE),
    K_MLE_plot = ifelse(is_extreme, NA_real_, K_MLE)  #
  )
K_summary <- K_noodd %>%
  group_by(param_set) %>%
  summarise(
    inf_ratio = mean(is_extreme),
    median_K_MLE = median(K_MLE_plot, na.rm = TRUE),
    true_K       = first(K),  # K is constant within a param_set
    .groups = "drop"
  )
ggplot(K_summary, aes(param_set, y = inf_ratio)) +
  geom_bar(stat = "identity", fill = "#d6cdb8", color = NA) +
  # make y-axis in percentage
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Parameter Set",
    y = "Proportion of extreme estimates"
  ) +
  theme_bw(base_size = 12)
# 2. Faceted histogram / density-style plot across 48 settings
ggplot(K_noodd, aes(x = K_MLE_plot)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "#d6cdb8",
                 color = NA,
                 na.rm = TRUE) +
  # median of MLEs (black)
  geom_vline(data = K_summary,
             aes(xintercept = median_K_MLE),
             linewidth = 0.6,
             #linetype = "dashed",
             color = "black") +
  # true value (blue, dashed)
  geom_vline(data = K_summary,
             aes(xintercept = true_K),
             linewidth = 0.6,
             linetype = "dashed",
             color = "steelblue") +
  facet_wrap(~ param_set, scales = "free_x", ncol = 6) +
  labs(
    x = expression(K),
    y = "Density"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

# gam ---------------------------------------------------------------------

cutoff <- 7
gam_noodd <- mle_true_bias %>%
  select(param_set, gam_MLE, gam, bias_gam) %>%
  mutate(
    is_extreme = bias_gam > cutoff * gam | !is.finite(gam_MLE),
    gam_MLE_plot = ifelse(is_extreme, NA_real_, gam_MLE)  #
  )
gam_summary <- gam_noodd %>%
  group_by(param_set) %>%
  summarise(
    inf_ratio = mean(is_extreme),
    median_gam_MLE = median(gam_MLE_plot, na.rm = TRUE),
    true_gam       = first(gam),  # gam is constant within a param_set
    .groups = "drop"
  )
ggplot(gam_summary, aes(param_set, y = inf_ratio)) +
  geom_bar(stat = "identity", fill = "#d6cdb8", color = NA) +
  # make y-axis in percentage
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Parameter Set",
    y = "Proportion of extreme estimates"
  ) +
  theme_bw(base_size = 12)
# 2. Faceted histogram / density-style plot across 48 settings
ggplot(gam_noodd, aes(x = gam_MLE_plot)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "#d6cdb8",
                 color = NA,
                 na.rm = TRUE) +
  # median of MLEs (black)
  geom_vline(data = gam_summary,
             aes(xintercept = median_gam_MLE),
             linewidth = 0.6,
             #linetype = "dashed",
             color = "black") +
  # true value (blue, dashed)
  geom_vline(data = gam_summary,
             aes(xintercept = true_gam),
             linewidth = 0.6,
             linetype = "dashed",
             color = "steelblue") +
  facet_wrap(~ param_set, scales = "free_x", ncol =6) +
  labs(
    x = expression(gamma),
    y = "Density"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )


# if don't exclude odd values

ggplot(mle_true_bias, aes(x = gam_MLE)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "#d6cdb8",
                 color = NA,
                 na.rm = TRUE) +
  facet_wrap(~ param_set, scales = "free_x") +
  labs(
    x = expression(gamma^" MLE"),
    y = "Density"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )




# combine 5 parameters without odd values ---------------------------------

library(dplyr)
library(ggplot2)
library(rlang)

plot_param_mle <- function(data,
                           param = c("lac", "mu", "K", "gam", "laa"),
                           type = c("hist", "density"),
                           ncol = 6) {

  param <- match.arg(param)
  type  <- match.arg(type)

  # Construct column names
  mle_col  <- paste0(param, "_MLE")
  true_col <- param

  # Check existence
  if (!mle_col %in% names(data))
    stop("Column ", mle_col, " not found.")

  if (!true_col %in% names(data))
    stop("Column ", true_col, " not found.")

  # Select only what we need
  df <- data %>%
    dplyr::select(param_set, all_of(mle_col), all_of(true_col))

  n_total <- nrow(df)
  n_na    <- sum(is.na(df[[mle_col]]))

  # Remove NAs
  df_clean <- df %>% filter(!is.na(.data[[mle_col]]))

  # Summary values per param_set
  summary_df <- df_clean %>%
    group_by(param_set) %>%
    summarise(
      median_mle = median(.data[[mle_col]]),
      true_val   = first(.data[[true_col]]),
      .groups = "drop"
    )

  # Base plot
  aes_mle <- aes(x = .data[[mle_col]])
  p <- ggplot(df_clean, aes_mle)

  if (type == "hist") {
    p <- p +
      geom_histogram(aes(y = ..density..),
                     bins = 30,
                     fill = "#d6cdb8",
                     color = NA)
  } else if (type == "density") {
    p <- p +
      geom_density(fill = "#d6cdb8",
                   alpha = 0.6,
                   linewidth = 0.8,
                   na.rm = TRUE)
  }

  # Parameter-specific axis label (LaTeX-style)
  param_label <- switch(param,
                        "lac" = expression(lambda[c]),
                        "mu"  = expression(mu),
                        "K"   = expression(K),
                        "gam" = expression(gamma),
                        "laa" = expression(lambda[a]),
                        param # fallback plain text
  )

  p <- p +
    geom_vline(data = summary_df,
               aes(xintercept = median_mle),
               linewidth = 0.6,
               color = "black") +
    geom_vline(data = summary_df,
               aes(xintercept = true_val),
               linewidth = 0.6,
               linetype = "dashed",
               color = "steelblue") +
    facet_wrap(~ param_set, scales = "free_x", ncol = ncol) +
    labs(
      x = bquote(MLE ~ of ~ .(param_label)),
      y = "Density",
      subtitle = if (n_na > 0)
        paste0(n_na, " NA values removed (", round(100 * n_na/n_total, 1), "%)")
      else NULL,
      title = NULL
    ) +
    theme_classic(base_size = 11) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(0.8, "lines"),
      plot.subtitle = element_text(size = 9, hjust = 0.5)
    )

  return(p)
}

plot_param_mle(mle_true_bias, param = "lac", type = "density")
plot_param_mle(mle_true_bias, param = "mu", type = "density")
plot_param_mle(mle_true_bias, param = "K", type = "density")
plot_param_mle(mle_true_bias, param = "gam", type = "density")
plot_param_mle(mle_true_bias, param = "laa", type = "density")
