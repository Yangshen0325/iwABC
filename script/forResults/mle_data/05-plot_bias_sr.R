
## Plot the absolute difference (bias) against species richness

rm(list = ls())

library(dplyr)
library(ggplot2)




# don't run, data saved ---------------------------------------------------

# combine the difference data with species richness

# read the data with param_set, true values, estimates, absolute bias
mle_true_bias <- readRDS("script/forResults/mle_data/mle_true_bias.rds")

# read the SPI summary statistics
SPI_all_stats <- readRDS("script/forResults/obs_data/SPI_all_stats.rds")

spi_total_sp <- SPI_all_stats %>%
  mutate(param_set = as.integer(stringr::str_extract(param_set, "\\d+")),
         total_sp = num_nonend_sim + num_sington_sim + num_multi_sim) %>%
  select(param_set, total_sp)

mle_true_bias2 <- mle_true_bias %>%
  select(
    param_set,
    lac, mu, K, gam, laa,          # true values
    lac_MLE, mu_MLE, K_MLE, gam_MLE, laa_MLE
  ) %>%
  mutate(
    # raw signed bias (MLE - true), not abs()
    diff_lac = lac_MLE  - lac,
    diff_mu  = mu_MLE   - mu,
    diff_K   = K_MLE    - K,
    diff_gam = gam_MLE  - gam,
    diff_laa = laa_MLE  - laa
  )

combined_diff_total <- bind_cols(
  mle_true_bias2,
  spi_total_sp %>% select(-param_set)
)

saveRDS(combined_diff_total, "script/forResults/mle_data/diff_richness.rds")



#  diff against total species, without odd estimates ----------------------


# Read data
combined_diff_total <- readRDS("script/forResults/mle_data/diff_richness.rds")


cutoff <- 7
lac_noodd <- combined_diff_total %>%
  select(param_set, lac_MLE, lac, diff_lac, total_sp) %>%
  mutate(
    is_extreme = abs(diff_lac) > cutoff * lac | !is.finite(lac_MLE), # if the estimates over 6 times of true
    lac_diff_plot = ifelse(is_extreme, NA_real_, diff_lac),
    total_sp_plot = ifelse(is_extreme, NA_real_, total_sp)
  )

p_bias_sr <- ggplot(lac_noodd, aes(x = total_sp_plot, y = abs(lac_diff_plot))) +
  geom_hline(yintercept = 0,
             linewidth = 0.4,
             linetype = "dashed",
             colour   = "grey40") +
  geom_point(alpha = 0.25,
             shape  = 16,
             stroke = 0) +
  # geom_smooth(method = "loess",
  #             formula = y ~ x,
  #             linewidth = 0.8,
  #             se = TRUE,
  #             colour = "black",
  #             fill   = "grey80") +
  labs(
    x = "Species richness",
    # y-axis using "delta lambda^c" to represent bias
    y = expression("Bias in " * lambda^c * " |(MLE - true)|")
    #title = expression("Cladogenesis rate " * lambda^c)
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(r = 6))
  )

p_bias_sr


# For mu

cutoff <- 1.0
mu_noodd <- combined_diff_total %>%
  select(param_set, mu_MLE, mu, diff_mu, total_sp) %>%
  mutate(
    is_extreme = abs(diff_mu) > cutoff | !is.finite(mu_MLE), # if the estimates over 6 times of true
    mu_diff_plot = ifelse(is_extreme, NA_real_, diff_mu),
    total_sp_plot = ifelse(is_extreme, NA_real_, total_sp)
  )
p_bias_sr_mu <- ggplot(mu_noodd, aes(x = total_sp_plot, y = abs(mu_diff_plot))) +
  geom_hline(yintercept = 0,
             linewidth = 0.4,
             linetype = "dashed",
             colour   = "grey40") +
  geom_point(alpha = 0.25,
             shape  = 16,
             stroke = 0) +
  # geom_smooth(method = "loess",
  #             formula = y ~ x,
  #             linewidth = 0.8,
  #             se = TRUE,
  #             colour = "black",
  #             fill   = "grey80") +
  labs(
    x = "Species richness",
    # y-axis using "delta mu" to represent bias
    y = expression("Bias in " * mu * " |(MLE - true)|")
    #title = expression("Extinction rate " * mu)
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(r = 6))
  )
p_bias_sr_mu

# K
cutoff <- 1.5
K_noodd <- combined_diff_total %>%
  select(param_set, K_MLE, K, diff_K, total_sp) %>%
  mutate(
    is_extreme = abs(diff_K) > cutoff *K | !is.finite(K_MLE), # if the estimates over 6 times of true
    K_diff_plot = ifelse(is_extreme, NA_real_, diff_K),
    total_sp_plot = ifelse(is_extreme, NA_real_, total_sp)
  )
p_bias_sr_K <- ggplot(K_noodd, aes(x = total_sp_plot, y = abs(K_diff_plot))) +
  geom_hline(yintercept = 0,
             linewidth = 0.4,
             linetype = "dashed",
             colour   = "grey40") +
  geom_point(alpha = 0.25,
             shape  = 16,
             stroke = 0) +
  # geom_smooth(method = "loess",
  #             formula = y ~ x,
  #             linewidth = 0.8,
  #             se = TRUE,
  #             colour = "black",
  #             fill   = "grey80") +
  labs(
    x = "Species richness",
    # y-axis using "delta K" to represent bias
    y = expression("Bias in K |(MLE - true)|")
    #title = "Carrying capacity K"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(r = 6))
  )
p_bias_sr_K

# gam
cutoff <- 7
gam_noodd <- combined_diff_total %>%
  select(param_set, gam_MLE, gam, diff_gam, total_sp) %>%
  mutate(
    is_extreme = abs(diff_gam) > cutoff * gam | !is.finite(gam_MLE), # if the estimates over 6 times of true
    gam_diff_plot = ifelse(is_extreme, NA_real_, diff_gam),
    total_sp_plot = ifelse(is_extreme, NA_real_, total_sp)
  )
p_bias_sr_gam <- ggplot(gam_noodd, aes(x = total_sp_plot, y = abs(gam_diff_plot))) +
  geom_hline(yintercept = 0,
             linewidth = 0.4,
             linetype = "dashed",
             colour   = "grey40") +
  geom_point(alpha = 0.25,
             shape  = 16,
             stroke = 0) +
  # geom_smooth(method = "loess",
  #             formula = y ~ x,
  #             linewidth = 0.8,
  #             se = TRUE,
  #             colour = "black",
  #             fill   = "grey80") +
  labs(
    x = "Species richness",
    # y-axis using "delta gam" to represent bias
    y = expression("Bias in " * gamma * " |(MLE - true)|")
    #title = expression
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(r = 6))
  )
p_bias_sr_gam

# laa
cutoff <- 6
laa_noodd <- combined_diff_total %>%
  select(param_set, laa_MLE, laa, diff_laa, total_sp) %>%
  mutate(
    is_extreme = abs(diff_laa) > cutoff * laa | !is.finite(laa_MLE), # if the estimates over 6 times of true
    laa_diff_plot = ifelse(is_extreme, NA_real_, diff_laa),
    total_sp_plot = ifelse(is_extreme, NA_real_, total_sp)
  )
p_bias_sr_laa <- ggplot(laa_noodd, aes(x = total_sp_plot, y = abs(laa_diff_plot))) +
  geom_hline(yintercept = 0,
             linewidth = 0.4,
             linetype = "dashed",
             colour   = "grey40") +
  geom_point(alpha = 0.25,
             shape  = 16,
             stroke = 0) +
  # geom_smooth(method = "loess",
  #             formula = y ~ x,
  #             linewidth = 0.8,
  #             se = TRUE,
  #             colour = "black",
  #             fill   = "grey80") +
  labs(
    x = "Species richness",
    # y-axis using "delta laa" to represent bias
    y = expression("Bias in " * lambda^a * " |(MLE - true)|")
    #title = expression("Anagenesis rate " * lambda^a)
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 6)),
    axis.title.y = element_text(margin = margin(r = 6))
  )
p_bias_sr_laa

# combine the 5 plots into one figure
library(gridExtra)
library(grid)
library(cowplot)
combined_plot <- plot_grid(
  p_bias_sr,
  p_bias_sr_mu,
  p_bias_sr_K,
  p_bias_sr_gam,
  p_bias_sr_laa,
  ncol = 2,
  labels = c("A", "B", "C", "D", "E"),
  label_size = 14,
  align = "hv"
)

combined_plot










#  diff against total species, try log scale ------------------------------





# diff against total species, with all estimates, not nice ----------------


library(dplyr)

mle_true_bias2_rep <- mle_true_bias2 %>%
  group_by(param_set) %>%
  arrange(param_set, .by_group = TRUE) %>%   # ensure stable order within param_set
  mutate(rep_id = row_number()) %>%
  ungroup()

spi_total_sp_rep <- spi_total_sp %>%
  group_by(param_set) %>%
  arrange(param_set, .by_group = TRUE) %>%   # same idea here
  mutate(rep_id = row_number()) %>%
  ungroup()

combined_diff_total <- bind_cols(
  mle_true_bias2,
  spi_total_sp %>% select(-param_set)
)

library(dplyr)
library(ggplot2)
library(rlang)

# General function: bias (diff_*) vs total_sp
plot_bias_vs_richness <- function(data,
                                  diff_var,
                                  y_lab,
                                  title = NULL,
                                  point_alpha = 0.25) {
  diff_sym <- ensym(diff_var)

  ggplot(data, aes(x = total_sp, y = !!diff_sym)) +
    # reference line: no bias
    geom_hline(yintercept = 0,
               linewidth = 0.4,
               linetype = "dashed",
               colour   = "grey40") +

    # points
    geom_point(alpha = point_alpha,
               shape  = 16,
               stroke = 0) +

    # smooth trend (LOESS)
    geom_smooth(method = "loess",
                formula = y ~ x,
                linewidth = 0.8,
                se = TRUE,
                colour = "black",
                fill   = "grey80") +

    labs(
      x = "Total number of species on island",
      y = y_lab,
      title = title
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title   = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6))
    )
}

p_lac <- plot_bias_vs_richness(
  combined_diff_total,
  diff_lac,
  y_lab = expression("Bias in " * lambda^c * " (MLE - true)"),
  title = expression("Cladogenesis rate " * lambda^c)
)
p_lac
p_mu <- plot_bias_vs_richness(
  combined_diff_total,
  diff_mu,
  y_lab = expression("Bias in " * mu * " (MLE - true)"),
  title = expression("Extinction rate " * mu)
)

p_K <- plot_bias_vs_richness(
  combined_diff_total,
  diff_K,
  y_lab = "Bias in K (MLE - true)",
  title = "Carrying capacity K"
)

p_gam <- plot_bias_vs_richness(
  combined_diff_total,
  diff_gam,
  y_lab = expression("Bias in " * gamma * " (MLE - true)"),
  title = expression("Immigration rate " * gamma)
)

p_laa <- plot_bias_vs_richness(
  combined_diff_total,
  diff_laa,
  y_lab = expression("Bias in " * lambda^a * " (MLE - true)"),
  title = expression("Anagenesis rate " * lambda^a)
)
p_mu
p_K
p_gam
p_laa

p_lac + p_mu + p_K + p_gam + p_laa

