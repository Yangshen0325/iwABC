


# Scatter plot true values and estimates for each parameter.
# e.g., when lambda^c = 0.4, what is like of estimates (mle and abc) for lambda^c?
# Show median and IQR (Q1, Q3) of estimates for each true value
# Note: MLE v.s. ABC-SS1,2,3,4, exclude ABC-SS0 due to low replications (10) and small iteration (10).

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggh4x)




# Don't run, data saved ---------------------------------------------------


# Read data

abc_true_bias_ss1 <- readRDS("script/forResults/abc_data/abc_true_bias_ss1.rds")

abc_true_bias_ss2 <- readRDS("script/forResults/abc_data/abc_true_bias_ss2.rds")

abc_true_bias_ss3 <- readRDS("script/forResults/abc_data/abc_true_bias_ss3.rds")

abc_true_bias_ss4 <- readRDS("script/forResults/abc_data/abc_true_bias_ss4.rds")

mle_true_bias <- readRDS("script/forResults/mle_data/mle_true_bias.rds")


# Convert one df to long: true vs est for each parameter
to_long <- function(df, method, est_suffix) {
  df %>%
    mutate(netdiv = lac - mu) %>%
    select(param_set, lac, mu, K, gam, laa,
           all_of(paste0(c("lac","mu","K","gam","laa"), "_", est_suffix)),
           netdiv, bias_netdiv) %>%
    pivot_longer(cols = c(lac, mu, K, gam, laa, netdiv),
                 names_to = "param", values_to = "true") %>%
    mutate(
      est = dplyr::case_when(
        param == "lac" ~ .data[[paste0("lac_", est_suffix)]],
        param == "mu"  ~ .data[[paste0("mu_",  est_suffix)]],
        param == "K"   ~ .data[[paste0("K_",   est_suffix)]],
        param == "gam" ~ .data[[paste0("gam_", est_suffix)]],
        param == "laa" ~ .data[[paste0("laa_", est_suffix)]],
        param == "netdiv" ~ .data[["bias_netdiv"]]
      ),
      method = method
    ) %>%
    select(param_set, param, true, est, method)
}

mle_long <- to_long(mle_true_bias, "MLE", est_suffix = "MLE")
abc1_long <- to_long(abc_true_bias_ss1, "ABC-SS1", est_suffix = "ABC")
abc2_long <- to_long(abc_true_bias_ss2, "ABC-SS2", est_suffix = "ABC")
abc3_long <- to_long(abc_true_bias_ss3, "ABC-SS3", est_suffix = "ABC")
abc4_long <- to_long(abc_true_bias_ss4, "ABC-SS4", est_suffix = "ABC")

all_long <- bind_rows(mle_long, abc1_long, abc2_long, abc3_long, abc4_long)

sum_df <- all_long %>%
  group_by(method, param, true) %>%
  summarise(
    q25 = quantile(est, 0.25, na.rm = TRUE),
    med = median(est, na.rm = TRUE),
    q75 = quantile(est, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    param_label = dplyr::recode(
      param,
      lac = "lambda^c",
      mu  = "mu",
      K   = "K",
      gam = "gamma",
      laa = "lambda^a",
      netdiv = "r[net]"
    )
  )
saveRDS(sum_df, "script/forResults/all_medQ1Q3_mle_abc1234.rds")



# plot --------------------------------------------------------------------
rm(list = ls())
# Read summary data
sum_df <- readRDS("script/forResults/all_medQ1Q3_mle_abc1234.rds")
parameter_space <- read_csv("data/parameter_space.csv")

panel_order <- c(
  "lambda^c",  # λ^c
  "mu",        # μ
  "K",         # K
  "gamma",     # γ
  "lambda^a",  # λ^a
  "r[net]"     # r_net
)

sum_df2 <- sum_df %>%
  mutate(
    q25 = unname(q25),
    med = unname(med),
    q75 = unname(q75),
    param_label = recode(
      param,
      lac    = "lambda^c",
      mu     = "mu",
      K      = "K",
      gam    = "gamma",
      laa    = "lambda^a",
      netdiv = "r[net]"
    ),
    param_label = factor(
      param_label,
      levels = panel_order
    ),
    # key step: remove floating-point noise ONLY for netdiv
    true_grp = if_else(param == "netdiv", round(true, 6), true)
  )


sum_df_plot <- sum_df2 %>%
  group_by(method, param, true_grp, param_label) %>%
  summarise(
    q25 = mean(q25, na.rm = TRUE),
    med = mean(med, na.rm = TRUE),
    q75 = mean(q75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(true = true_grp)

true_cross_df <- bind_rows(
  parameter_space %>% transmute(param = "lac",    true = lac),
  parameter_space %>% transmute(param = "mu",     true = mu),
  parameter_space %>% transmute(param = "K",      true = K),
  parameter_space %>% transmute(param = "gam",    true = gam),
  parameter_space %>% transmute(param = "laa",    true = laa),
  parameter_space %>% transmute(param = "netdiv", true = lac - mu)
) %>%
  distinct(param, true) %>%
  mutate(
    est = true,  # crosses lie on the 1:1 line
    param_label = recode(
      param,
      lac    = "lambda^c",
      mu     = "mu",
      K      = "K",
      gam    = "gamma",
      laa    = "lambda^a",
      netdiv = "r[net]"
    ),
    param_label = factor(param_label, levels = panel_order)
  )

x_range_df <- sum_df_plot %>%
  group_by(param_label) %>%
  summarise(
    xr = max(true, na.rm = TRUE) - min(true, na.rm = TRUE),
    .groups = "drop"
  )

method_offsets <- tibble::tibble(
  method = sort(unique(sum_df_plot$method)),
  offset_id = seq_along(method)
) %>%
  mutate(offset_id = offset_id - mean(offset_id))

sum_df_plot <- sum_df_plot %>%
  left_join(x_range_df, by = "param_label") %>%
  left_join(method_offsets, by = "method") %>%
  mutate(
    # 3% of panel x-range → tune between 0.02–0.05
    true_dodged = true + offset_id * 0.03 * xr
  )


p <- ggplot(sum_df_plot, aes(x = true_dodged, y = med, color = method)) +

  # 1:1 line
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    linewidth = 0.6,
    color = "grey50"
  ) +

  # IQR (Q1–Q3) + median
  geom_pointrange(
    aes(ymin = q25, ymax = q75),
    linewidth = 0.7,
    size = 0.15
  ) +

  # true-value crosses
  geom_point(
    data = true_cross_df,
    aes(x = true, y = est),
    inherit.aes = FALSE,
    shape = 4,          # ×
    size = 2.7,
    stroke = 0.95,
    color = "black"
  ) +

  facet_wrap(
    ~ param_label,
    scales = "free",
    nrow = 2,
    labeller = label_parsed
  ) +

  # leave some space before/after true values on x (per facet, since scales="free")
  scale_x_continuous(expand = expansion(mult = 0.2)) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +

  # colour palette (Okabe–Ito; adjust if you have different method names)
  scale_colour_manual(
    name = "Inference method",
    values = c(
      "ABC-SS1" = "#D55E00",
      "ABC-SS2" = "#009E73",
      "ABC-SS3" = "#7570B3",
      "ABC-SS4" = "#0072B2",
      "MLE"     = "#CC79A7"
    )
  ) +

  labs(
    x = "True value (pseudo-empirical)",
    y = "Estimated parameter value"
  ) +

  theme_bw(base_size = 12) +
  theme(
    panel.grid       = element_blank(),
    strip.background = element_rect(fill = "grey90", colour = "grey50", linewidth = 0.6),
    strip.text       = element_text(face = "bold"),
    axis.title       = element_text(face = "bold"),
    axis.text        = element_text(colour = "black"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    panel.border     = element_rect(colour = "black", linewidth = 0.6)
  )

p

# save it as publishable figure
ggsave("script/forResults/scatter_true_abc_mle.pdf",
       plot = p,
       device   = cairo_pdf,   # ensures proper font embedding
       width = 18, height = 10,
       units = "cm",
       dpi = 300)
