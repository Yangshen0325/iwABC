
#### MLE results
# Show the difference results in a given combination parameters setting.

rm(list = ls())

library(tidyverse)
# Read data
mle_true_bias <- readRDS("script/forResults/mle_data/mle_true_bias.rds")

# --- 1. Keep only columns we need ---
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


plot_df_bias <- mle_true_bias2 %>%
  group_by(lac, mu, gam, laa, K) %>%
  summarise(
    median_lac = median(lac_MLE, na.rm = TRUE),
    lower_lac = quantile(lac_MLE, 0.25, na.rm = TRUE),
    upper_lac = quantile(lac_MLE, 0.75, na.rm = TRUE),

    median_mu = median(mu_MLE, na.rm = TRUE),
    lower_mu = quantile(mu_MLE, 0.25, na.rm = TRUE),
    upper_mu = quantile(mu_MLE, 0.75, na.rm = TRUE),

    median_K = median(K_MLE, na.rm = TRUE),
    lower_K = quantile(K_MLE, 0.25, na.rm = TRUE),
    upper_K = quantile(K_MLE, 0.75, na.rm = TRUE),

    median_gam = median(gam_MLE, na.rm = TRUE),
    lower_gam = quantile(gam_MLE, 0.25, na.rm = TRUE),
    upper_gam = quantile(gam_MLE, 0.75, na.rm = TRUE),

    median_laa = median(laa_MLE, na.rm = TRUE),
    lower_laa = quantile(laa_MLE, 0.25, na.rm = TRUE),
    upper_laa = quantile(laa_MLE, 0.75, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  mutate(
    # facet labels
    lac_f = factor(lac, levels = sort(unique(lac)),
                   labels = paste0("lambda^c==", sort(unique(lac)))),
    mu_f  = factor(mu,  levels = sort(unique(mu)),
                   labels = paste0("mu==", sort(unique(mu)))),
    gam_f = factor(gam, levels = sort(unique(gam)),
                   labels = paste0("gamma==", sort(unique(gam)))),
    laa_f = factor(laa, levels = sort(unique(laa)),
                   labels = paste0("lambda^a==", sort(unique(laa)))),
    K_f   = factor(K)
  )

plot_long <- plot_df_bias %>%
  pivot_longer(
    cols      = starts_with("median_"),
    names_to  = "param",
    names_prefix = "median_",
    values_to = "median"
  ) %>%
  mutate(
    lower = case_when(
      param == "lac" ~ lower_lac,
      param == "mu"  ~ lower_mu,
      param == "gam" ~ lower_gam,
      param == "laa" ~ lower_laa,
      param == "K"   ~ lower_K
    ),
    upper = case_when(
      param == "lac" ~ upper_lac,
      param == "mu"  ~ upper_mu,
      param == "gam" ~ upper_gam,
      param == "laa" ~ upper_laa,
      param == "K"   ~ upper_K
    )
  ) %>%
  filter(param %in% c("lac", "mu", "gam", "laa")) %>%   # drop K if unwanted
  mutate(
    param = factor(param, levels = c("lac", "mu", "gam", "laa"))
  )


pd <- position_dodge(width = 0.5) # the distance bewteen points

p_difference_mle_true <- ggplot(
  plot_long,
  aes(
    x      = K_f,               # x = K (as in previous good figure)
    y      = median,
    colour = param,
    shape  = K_f,
    group  = interaction(param, K_f)
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +   # zero bias reference
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = pd,
    linewidth = 0.4
  ) +
  facet_grid(
    gam_f + laa_f ~ lac_f + mu_f,
    labeller = label_parsed
  ) +
  scale_colour_manual(
    values = c(
      lac = "#d73027",    # λᶜ
      mu  = "#1a9850",    # μ
      gam = "#4575b4",    # γ
      laa = "#762a83"     # λᵃ
    ),
    labels = c(
      lac = expression(lambda^c),
      mu  = expression(mu),
      gam = expression(gamma),
      laa = expression(lambda^a)
    ),
    name = "Parameter"
  ) +
  scale_shape_manual(
    values = c("20" = 16, "50" = 17, "100" = 15),
    name   = "K"
  ) +
  labs(
    x = "K",
    y = "Difference (MLE − true)",
   # title = "Bias of MLE estimates under 16 combinations of DAISIE parameters",
    #subtitle = "Dots = median bias; lines = 2.5–97.5% intervals"
  ) +
  theme_bw(base_size = 12) +
  theme(
    #panel.grid       = element_blank(),
    panel.spacing    = unit(0.5, "lines"),
    strip.background = element_rect(colour = "grey30", fill = "grey95", linewidth = 0.8),
    strip.text       = element_text(face = "bold", size = 8),
    axis.title       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.border     = element_rect(colour = "black", fill = NA, linewidth = 1)
  )

ggsave("script/forResults/mle_data/p_difference_mle_true.pdf",
       plot = p_difference_mle_true,
       width = 8.5,        # Standard paper width
       height = 6,
       device = cairo_pdf, # Better font handling
       dpi = 300)


