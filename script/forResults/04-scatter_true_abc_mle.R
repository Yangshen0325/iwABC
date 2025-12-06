

# Scatter plot true values and estimates for each parameter.
# e.g., when lambda^c = 0.4, what is like of estimates (mle and abc) for lambda^c?
# Show median and IQR (Q1, Q3) of estimates for each true value

library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())



# Don't run, data saved ---------------------------------------------------

# To get all median, Q1, Q3 for MLE and ABC methods in the case of:
# lac=0.4, 0.7 respectively
# mu=0, 0.3 respectively
# K=20, 50, 100 respectively
# gam=0.001, 0.002 respectively
# laa=0.1, 1.0 respectively

# Read data
abc_true_bias_ss1 <- readRDS("script/forResults/abc_data/abc_true_bias_ss1.rds")

abc_true_bias_ss2 <- readRDS("script/forResults/abc_data/abc_true_bias_ss2.rds")

abc_true_bias_ss3 <- readRDS("script/forResults/abc_data/abc_true_bias_ss3.rds")

mle_true_bias <- readRDS("script/forResults/mle_data/mle_true_bias.rds")


# Function to get medidan, 25th, 75th for data

get_summary <- function(data, est_type = c("MLE", "ABC")) {

  est_type <- match.arg(est_type)  # ensures only allowed values

  # Helper to pick the right column name, e.g. "lac_MLE" or "lac_ABC"
  col_name <- function(par) paste0(par, "_", est_type)

  summary_info <- data %>%
    summarise(
      # lac
      lac0.4_q25 = quantile(.data[[col_name("lac")]][lac == 0.4], 0.25, na.rm = TRUE),
      lac0.4_med = median  (.data[[col_name("lac")]][lac == 0.4],         na.rm = TRUE),
      lac0.4_q75 = quantile(.data[[col_name("lac")]][lac == 0.4], 0.75,   na.rm = TRUE),

      lac0.7_q25 = quantile(.data[[col_name("lac")]][lac == 0.7], 0.25, na.rm = TRUE),
      lac0.7_med = median  (.data[[col_name("lac")]][lac == 0.7],         na.rm = TRUE),
      lac0.7_q75 = quantile(.data[[col_name("lac")]][lac == 0.7], 0.75,   na.rm = TRUE),

      # mu
      mu0_q25   = quantile(.data[[col_name("mu")]][mu == 0],   0.25, na.rm = TRUE),
      mu0_med   = median  (.data[[col_name("mu")]][mu == 0],           na.rm = TRUE),
      mu0_q75   = quantile(.data[[col_name("mu")]][mu == 0],   0.75,   na.rm = TRUE),

      mu0.3_q25 = quantile(.data[[col_name("mu")]][mu == 0.3], 0.25, na.rm = TRUE),
      mu0.3_med = median  (.data[[col_name("mu")]][mu == 0.3],         na.rm = TRUE),
      mu0.3_q75 = quantile(.data[[col_name("mu")]][mu == 0.3], 0.75,   na.rm = TRUE),

      # K
      K20_q25  = quantile(.data[[col_name("K")]][K == 20], 0.25, na.rm = TRUE),
      K20_med  = median  (.data[[col_name("K")]][K == 20],         na.rm = TRUE),
      K20_q75  = quantile(.data[[col_name("K")]][K == 20], 0.75,   na.rm = TRUE),

      K50_q25  = quantile(.data[[col_name("K")]][K == 50], 0.25, na.rm = TRUE),
      K50_med  = median  (.data[[col_name("K")]][K == 50],         na.rm = TRUE),
      K50_q75  = quantile(.data[[col_name("K")]][K == 50], 0.75,   na.rm = TRUE),

      K100_q25 = quantile(.data[[col_name("K")]][K == 100], 0.25, na.rm = TRUE),
      K100_med = median  (.data[[col_name("K")]][K == 100],         na.rm = TRUE),
      K100_q75 = quantile(.data[[col_name("K")]][K == 100], 0.75,   na.rm = TRUE),

      # gam
      gam0.001_q25 = quantile(.data[[col_name("gam")]][gam == 0.001], 0.25, na.rm = TRUE),
      gam0.001_med = median  (.data[[col_name("gam")]][gam == 0.001],         na.rm = TRUE),
      gam0.001_q75 = quantile(.data[[col_name("gam")]][gam == 0.001], 0.75,   na.rm = TRUE),

      gam0.002_q25 = quantile(.data[[col_name("gam")]][gam == 0.002], 0.25, na.rm = TRUE),
      gam0.002_med = median  (.data[[col_name("gam")]][gam == 0.002],         na.rm = TRUE),
      gam0.002_q75 = quantile(.data[[col_name("gam")]][gam == 0.002], 0.75,   na.rm = TRUE),

      # laa
      laa0.1_q25 = quantile(.data[[col_name("laa")]][laa == 0.1], 0.25, na.rm = TRUE),
      laa0.1_med = median  (.data[[col_name("laa")]][laa == 0.1],         na.rm = TRUE),
      laa0.1_q75 = quantile(.data[[col_name("laa")]][laa == 0.1], 0.75,   na.rm = TRUE),

      laa1_q25  = quantile(.data[[col_name("laa")]][laa == 1.0], 0.25, na.rm = TRUE),
      laa1_med  = median  (.data[[col_name("laa")]][laa == 1.0],         na.rm = TRUE),
      laa1_q75  = quantile(.data[[col_name("laa")]][laa == 1.0], 0.75,   na.rm = TRUE)
    )

  return(summary_info)
}

summary_mle <- get_summary(mle_true_bias, est_type = "MLE")
summary_abc_ss1 <- get_summary(abc_true_bias_ss1, est_type = "ABC")
summary_abc_ss2 <- get_summary(abc_true_bias_ss2, est_type = "ABC")
summary_abc_ss3 <- get_summary(abc_true_bias_ss3, est_type = "ABC")

# Convert summaries to long format for plotting
# The data structure is like: par (e.g. lac), true (e.g. 0.4), stat (e.g. Q1), estimate, method (e.g. MLE)
# Funtion
make_long_summary <- function(summary_df, method_label) {
  summary_df %>%
    pivot_longer(everything(), names_to = "name", values_to = "estimate") %>%
    tidyr::extract(
      name,
      into  = c("par", "true", "stat"),
      regex = "^([A-Za-z]+)([0-9.]+)_(q25|med|q75)$"
    ) %>%
    mutate(
      true   = as.numeric(true),
      stat   = dplyr::recode(stat,
                             q25 = "Q1",
                             med = "Median",
                             q75 = "Q3"),
      method = method_label
    )
}

mle_long    <- make_long_summary(summary_mle,      "MLE")
abc1_long   <- make_long_summary(summary_abc_ss1, "ABC-SS1")
abc2_long   <- make_long_summary(summary_abc_ss2, "ABC-SS2")
abc3_long   <- make_long_summary(summary_abc_ss3, "ABC-SS3")
# Combine all summaries
all_summary_long <- bind_rows(mle_long, abc1_long, abc2_long, abc3_long)

# save it
saveRDS(all_summary_long, "script/forResults/all_medQ1Q3_mle_abc.rds")




# Process data, plot ------------------------------------------------------

# read data
# `all_summary_long` has columns: par, true, stat, estimate, method
# e.g. par = lac, true = 0.4, stat = Q1, estimate = 0.35, method = MLE

all_summary_long <- readRDS("script/forResults/all_medQ1Q3_mle_abc.rds")

all_summary_wide <- all_summary_long %>%
  pivot_wider(
    names_from  = stat,
    values_from = estimate
  )

all_summary_wide <- all_summary_wide %>%
  mutate(
    param_label = dplyr::recode(
      par,
      lac = "lambda^c",
      mu  = "mu",
      K   = "K",
      gam = "gamma",
      laa = "lambda^a"
    )
  )

# one row per parameter × true value
truth_df <- all_summary_wide %>%
  dplyr::distinct(par, param_label, true)

p_estimates <- ggplot(all_summary_wide,
                      aes(x = true, y = Median, colour = method)) +
  # 1:1 line (perfect estimation)
  geom_abline(
    slope     = 1,
    intercept = 0,
    linetype  = "dashed",
    linewidth = 0.7,
    colour    = "grey55"
  ) +

  # median + IQR as vertical pointrange
  geom_pointrange(
    aes(ymin = Q1, ymax = Q3),
    linewidth = 0.7,   # thickness of the bar
    size      = 0.3,   # size of the point
    position  = position_identity()
  ) +

  # true values as black crosses
  geom_point(
    data        = truth_df,
    aes(x = true, y = true),
    inherit.aes = FALSE,
    shape       = 4,
    size        = 2.6,
    stroke      = 0.9,
    colour      = "black"
  ) +

  # facets by parameter, free y-scale but aligned x within each panel
  facet_wrap(
    ~ param_label,
    scales   = "free",
    nrow     = 2,
    labeller = label_parsed
  ) +

  # gentle, consistent expansions on both axes
  scale_x_continuous(
    expand = expansion(mult = c(0.03, 0.06)) # Adds 3% of the data range below the minimum value
  ) +                                       # Adds 6% of the data range above the maximum value
  scale_y_continuous(
    expand = expansion(mult = c(0.03, 0.08))
  ) +

  # colour palette (Okabe–Ito, colourblind-friendly)
  scale_colour_manual(
    name   = "Inference method",
    values = c(
      "ABC-SS1" = "#009E73",  # green
      "ABC-SS2" = "#E69F00",  # orange
      "ABC-SS3" = "#0072B2",  # blue
      "MLE"     = "#CC79A7"   # magenta
    )
  ) +

  labs(
    x = "True value (pesudo-empirical)",
    y = "Estimated parameter value"
  ) +

  # thicker, authorship-style theme
  theme_bw(base_size = 12) +
  theme(
    panel.grid       = element_blank(),
    strip.background = element_rect(fill = "grey90", colour = "grey50",
                                    linewidth = 0.6),
    strip.text       = element_text(face = "bold"),

    axis.title       = element_text(face = "bold"),
    axis.text        = element_text(colour = "black"),
    axis.title.x     = element_text(margin = margin(t = 6)),
    axis.title.y     = element_text(margin = margin(r = 6)),

    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    legend.key.width = unit(1.4, "lines"),
    legend.key.height= unit(0.9, "lines"),
    legend.margin    = margin(t = 2),

    plot.margin      = margin(6, 6, 10, 6),
    panel.border     = element_rect(colour = "black", linewidth = 0.6)
  )

p_estimates
