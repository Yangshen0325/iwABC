
## Visualise MLE lac estimates across param sets
# Show all results, but mark the proportion of extreme values per param set
# 04-plot_difference uses median and 25, 75 percentile and make them all in one plot.
# Here show the full distribution of estimates per param set (boxplot) but exclude extreme values

rm(list=ls())

library(dplyr)
library(ggplot2)
library(patchwork)


# read the data with param_set, true values, estimates, absolute bias
mle_true_bias <- readRDS("script/forResults/mle_data/mle_true_bias.rds")



# lac MLE across param_sets (boxplot) -------------------------------------


#  Order param_set by lac
order_tbl <- mle_true_bias %>%
  distinct(param_set, lac) %>%
  arrange(lac, param_set) %>%
  mutate(
    param_set_ord = factor(param_set, levels = param_set)
  ) %>%
  select(param_set, param_set_ord, lac_value = lac)



## Join order + flag extremes

cutoff <- 6

plot_df <- mle_true_bias %>%
  select(param_set, lac_MLE, lac, bias_lac) %>%
  left_join(order_tbl, by = "param_set") %>%
  mutate(
    is_extreme = bias_lac > cutoff * lac | !is.finite(lac_MLE) # if the estimates over 6 times of true
    #lac_MLE_plot = ifelse(is_extreme, NA_real_, lac_MLE)  #
  )


# Summarise extreme proportions per param_set & lac_value
ext_summary <- plot_df %>%
  group_by(param_set_ord, lac_value) %>%
  summarise(
    inf_ratio = mean(is_extreme),
    .groups   = "drop"
  ) %>%
  mutate(
    # fixed vertical positions by lac_value
    y_anno = ifelse(lac_value == 0.4, 3.8,
                    ifelse(lac_value == 0.7, 5, NA_real_)),
    # only show label if there is at least one extreme
    label  = ifelse(inf_ratio > 0,
                    paste0(round(100 * inf_ratio), "%"),
                    "0%")
  )



p_lac <- ggplot(plot_df,
       aes(x = param_set_ord,
           y = lac_MLE,
           fill = factor(lac_value))) +
  geom_boxplot(outlier.size = 0.4, na.rm = TRUE) +

  coord_cartesian(ylim = c(0, 5)) +

  ## true lac
  geom_point(
    data = distinct(plot_df, param_set_ord, lac_value),
    aes(x = param_set_ord, y = lac_value),
    inherit.aes = FALSE,
    colour = "blue", size = 1.8
  ) +

  # geom_segment(data = distinct(plot_df, param_set_ord, lac_value) %>%
  #                mutate(y_anno = case_when(
  #                  lac_value == 0.4 ~ 0.4 * cutoff,
  #                  lac_value == 0.7 ~ 0.7 * cutoff,
  #                  TRUE ~ NA_real_
  #                )) %>%
  #                filter(!is.na(y_anno)),
  #              aes(x = as.numeric(param_set_ord) - 0.4,
  #                  xend = as.numeric(param_set_ord) + 0.4,
  #                  y = y_anno, yend = y_anno),
  #              linetype = "dashed", color = "red", linewidth = 0.8,
  #              inherit.aes = FALSE) +
  #
  # geom_text(
  #   data = ext_summary,
  #   aes(x = param_set_ord,
  #       y = y_anno,
  #       label = label),
  #   inherit.aes = FALSE,
  #   size  = 2.1,
  #   colour  = "red",
  #   fontface = "bold"
  # ) +

  #scale_y_continuous(limits = c(0, 5)) +
  scale_fill_manual(
    values = c("0.4" = "grey60", "0.7" = "black"),
    name   = "True value"
  ) +
  ylab(bquote(lambda^c ~ "MLE")) +
  #xlab("Parameter Set") +
  xlab("") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x  = element_blank()
  )

p_lac

ggsave("script/forResults/mle_data/p_lac.pdf",
       plot = p_lac,
       width = 8.5,        # Standard paper width
       height = 3,
       device = cairo_pdf, # Better font handling
       dpi = 300)

# mu MLE across param_sets (boxplot)  --------------------------------------

#  Order param_set by mu
order_tbl <- mle_true_bias %>%
  distinct(param_set, mu) %>%
  arrange(mu, param_set) %>%
  mutate(
    param_set_ord = factor(param_set, levels = param_set)
  ) %>%
  select(param_set, param_set_ord, mu_value = mu)

## Join order + flag extremes
# node: one of extinction true values is 0. (0, 0.3)
# bias larger than num*true doesn't work

cutoff <- 1.0
plot_df <- mle_true_bias %>%
  select(param_set, mu_MLE, mu, bias_mu) %>%
  left_join(order_tbl, by = "param_set") %>%
  mutate(
    is_extreme = bias_mu > cutoff | !is.finite(mu_MLE)

  )


# Summarise extreme proportions per param_set & mu_value
ext_summary <- plot_df %>%
  group_by(param_set_ord, mu_value) %>%
  summarise(
    inf_ratio = mean(is_extreme),
    .groups   = "drop"
  ) %>%
  mutate(
    # fixed vertical positions by mu_value
    y_anno = ifelse(mu_value == 0, 1.1,
                    ifelse(mu_value == 0.3, 1.4, NA_real_)),
    # only show label if there is at least one extreme
    label  = ifelse(inf_ratio > 0,
                    paste0(round(100 * inf_ratio), "%"),
                    "0%")
  )


p_mu <- ggplot(plot_df,
               aes(x = param_set_ord,
                   y = mu_MLE,
                   fill = factor(mu_value))) +
  geom_boxplot(outlier.size = 0.4, na.rm = TRUE) +

  coord_cartesian(ylim = c(0, 1)) +

  ## true mu
  geom_point(
    data = distinct(plot_df, param_set_ord, mu_value),
    aes(x = param_set_ord, y = mu_value),
    inherit.aes = FALSE,
    colour = "blue", size = 1.8
  ) +

  # geom_segment(data = distinct(plot_df, param_set_ord, mu_value) %>%
  #                mutate(y_anno = case_when(
  #                  mu_value == 0 ~  cutoff,
  #                  mu_value == 0.3 ~  cutoff,
  #                  TRUE ~ NA_real_
  #                )) %>%
  #                filter(!is.na(y_anno)),
  #              aes(x = as.numeric(param_set_ord) - 0.4,
  #                  xend = as.numeric(param_set_ord) + 0.4,
  #                  y = y_anno, yend = y_anno),
  #              linetype = "dashed", color = "red", linewidth = 0.8,
  #              inherit.aes = FALSE) +
  #
  # # extreme proportions at y = 2 (mu = 0.4) and y = 3 (mu = 0.7)
  # geom_text(
  #   data = ext_summary,
  #   aes(x = param_set_ord,
  #       y = y_anno,
  #       label = label),
  #   inherit.aes = FALSE,
  #   size  = 2.1,
  #   colour  = "red",
  #   fontface = "bold"
  # ) +

  scale_fill_manual(
    values = c("0" = "grey60", "0.3" = "black"),
    name   = "True value"
  ) +
  ylab(bquote(mu ~ "MLE")) +
  xlab("") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x  = element_blank()
  )
p_mu
ggsave("script/forResults/mle_data/p_mu.pdf",
       plot = p_mu,
       width = 8.5,        # Standard paper width
       height = 3,
       device = cairo_pdf, # Better font handling
       dpi = 300)


# K MLE across param_sets (boxplot) ---------------------------------------



#  Order param_set by K
order_tbl <- mle_true_bias %>%
  distinct(param_set, K) %>%
  arrange(K, param_set) %>%
  mutate(
    param_set_ord = factor(param_set, levels = param_set)
  ) %>%
  select(param_set, param_set_ord, K_value = K)

## Join order + flag extremes
# 20, 50, 100
# cutoff = 2,3 doesn't improve much
cutoff <- 1.5

plot_df <- mle_true_bias %>%
  select(param_set, K_MLE, K, bias_K) %>%
  left_join(order_tbl, by = "param_set") %>%
  mutate(
    is_extreme = bias_K > cutoff * K | !is.finite(K_MLE)
    #K_MLE_plot = ifelse(is_extreme, NA_real_, K_MLE)  #
  )


# Summarise extreme proportions per param_set & K_value
ext_summary <- plot_df %>%
  group_by(param_set_ord, K_value) %>%
  summarise(
    inf_ratio = mean(is_extreme),
    .groups   = "drop"
  ) %>%
  mutate(
    # fixed vertical positions by K_value
    y_anno = ifelse(K_value == 20, 50,
                    ifelse(K_value == 50, 100, 175)),
    # only show label if there is at least one extreme
    label  = ifelse(inf_ratio > 0,
                    paste0(round(100 * inf_ratio), "%"),
                    "0%")
  )


p_K <- ggplot(plot_df,
              aes(x = param_set_ord,
                  y = K_MLE,
                  fill = factor(K_value))) +
  geom_boxplot(outlier.size = 0.4, na.rm = TRUE) +
  coord_cartesian(ylim = c(0, 125)) +

  ## true K
  geom_point(
    data = distinct(plot_df, param_set_ord, K_value),
    aes(x = param_set_ord, y = K_value),
    inherit.aes = FALSE,
    colour = "blue", size = 1.8
  ) +
  # geom_segment(data = distinct(plot_df, param_set_ord, K_value) %>%
  #                mutate(y_anno = case_when(
  #                  K_value == 20 ~  20*cutoff,
  #                  K_value == 50 ~  50*cutoff,
  #                  K_value == 100 ~  100*cutoff,
  #                  TRUE ~ NA_real_
  #                )) %>%
  #                filter(!is.na(y_anno)),
  #              aes(x = as.numeric(param_set_ord) - 0.4,
  #                  xend = as.numeric(param_set_ord) + 0.4,
  #                  y = y_anno, yend = y_anno),
  #              linetype = "dashed", color = "red", linewidth = 0.8,
  #              inherit.aes = FALSE) +
  #
  # # extreme proportions at y = 2 (K = 0.4) and y = 3 (K = 0.7)
  # geom_text(
  #   data = ext_summary,
  #   aes(x = param_set_ord,
  #       y = y_anno,
  #       label = label),
  #   inherit.aes = FALSE,
  #   size  = 2.1,
  #   colour  = "red",
  #   fontface = "bold"
  # ) +

  scale_fill_manual(
    values = c("20" = "grey60", "50" = "grey30", "100" = "black"),
    name   = "True value"
  ) +
  ylab("K MLE") +
  xlab("") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x  = element_blank()
  )

p_K

ggsave("script/forResults/mle_data/p_K.pdf",
       plot = p_K,
       width = 8.5,        # Standard paper width
       height = 3,
       device = cairo_pdf, # Better font handling
       dpi = 300)






# gam MLE across param_sets (boxplot) -------------------------------------


#  Order param_set by gam
order_tbl <- mle_true_bias %>%
  distinct(param_set, gam) %>%
  arrange(gam, param_set) %>%
  mutate(
    param_set_ord = factor(param_set, levels = param_set)
  ) %>%
  select(param_set, param_set_ord, gam_value = gam)

## Join order + flag extremes
# 0.001, 0.002
cutoff <- 7 # slightly better than 6

plot_df <- mle_true_bias %>%
  select(param_set, gam_MLE, gam, bias_gam) %>%
  left_join(order_tbl, by = "param_set") %>%
  mutate(
    is_extreme = bias_gam > cutoff * gam | !is.finite(gam_MLE)
    #gam_MLE_plot = ifelse(is_extreme, NA_real_, gam_MLE)  #
  )

# Summarise extreme proportions per param_set & gam_value
ext_summary <- plot_df %>%
  group_by(param_set_ord, gam_value) %>%
  summarise(
    inf_ratio = mean(is_extreme),
    .groups   = "drop"
  ) %>%
  mutate(
    # fixed vertical positions by gam_value
    y_anno = ifelse(gam_value == 0.001, 0.008,
                    ifelse(gam_value == 0.002, 0.015, NA_real_)),
    # only show label if there is at least one extreme
    label  = ifelse(inf_ratio > 0,
                    paste0(round(100 * inf_ratio), "%"),
                    "0%")
  )


p_gam <- ggplot(plot_df,
                aes(x = param_set_ord,
                    y = gam_MLE,
                    fill = factor(gam_value))) +
  geom_boxplot(outlier.size = 0.4, na.rm = TRUE) +
  coord_cartesian(ylim = c(0, 0.02)) +

  ## true gam
  geom_point(
    data = distinct(plot_df, param_set_ord, gam_value),
    aes(x = param_set_ord, y = gam_value),
    inherit.aes = FALSE,
    colour = "blue", size = 1.8
  ) +
  # geom_segment(data = distinct(plot_df, param_set_ord, gam_value) %>%
  #                mutate(y_anno = case_when(
  #                  gam_value == 0.001 ~  0.001*cutoff,
  #                  gam_value == 0.002 ~  0.002*cutoff,
  #                  TRUE ~ NA_real_
  #                )) %>%
  #                filter(!is.na(y_anno)),
  #              aes(x = as.numeric(param_set_ord) - 0.4,
  #                  xend = as.numeric(param_set_ord) + 0.4,
  #                  y = y_anno, yend = y_anno),
  #              linetype = "dashed", color = "red", linewidth = 0.8,
  #              inherit.aes = FALSE) +

  # extreme proportions at y = 2 (gam = 0.4) and y = 3 (gam = 0.7)
  # geom_text(
  #   data = ext_summary,
  #   aes(x = param_set_ord,
  #       y = y_anno,
  #       label = label),
  #   inherit.aes = FALSE,
  #   size  = 3.5,
  #   colour  = "red",
  #   fontface = "bold"
  # ) +

  scale_fill_manual(
    values = c("0.001" = "grey60",  "0.002" = "black"),
    name   = "True value"
  ) +
  ylab(bquote(gamma ~ "MLE")) +
  xlab("") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x  = element_blank()
  )

p_gam


ggsave("script/forResults/mle_data/p_gam.pdf",
       plot = p_gam,
       width = 8.5,        # Standard paper width
       height = 3,
       device = cairo_pdf, # Better font handling
       dpi = 300)



# laa MLE across param_sets (boxplot) -------------------------------------

#  Order param_set by laa
order_tbl <- mle_true_bias %>%
  distinct(param_set, laa) %>%
  arrange(laa, param_set) %>%
  mutate(
    param_set_ord = factor(param_set, levels = param_set)
  ) %>%
  select(param_set, param_set_ord, laa_value = laa)

## Join order + flag extremes
# 0.1 and 1.0
cutoff <- 6

plot_df <- mle_true_bias %>%
  select(param_set, laa_MLE, laa, bias_laa) %>%
  left_join(order_tbl, by = "param_set") %>%
  mutate(
    is_extreme = bias_laa > cutoff * laa | !is.finite(laa_MLE)

  )


# Summarise extreme proportions per param_set & laa_value
ext_summary <- plot_df %>%
  group_by(param_set_ord, laa_value) %>%
  summarise(
    inf_ratio = mean(is_extreme),
    .groups   = "drop"
  ) %>%
  mutate(
    # fixed vertical positions by laa_value
    y_anno = ifelse(laa_value == 0.1, 1.0,
                    ifelse(laa_value == 1, 6, NA_real_)),
    # only show label if there is at least one extreme
    label  = ifelse(inf_ratio > 0,
                    paste0(round(100 * inf_ratio), "%"),
                    "0%")
  )


p_laa <- ggplot(plot_df,
                aes(x = param_set_ord,
                    y = laa_MLE,
                    fill = factor(laa_value))) +
  geom_boxplot(outlier.size = 0.4, na.rm = TRUE) +
  coord_cartesian(ylim = c(0, 6)) +

  ## true laa
  geom_point(
    data = distinct(plot_df, param_set_ord, laa_value),
    aes(x = param_set_ord, y = laa_value),
    inherit.aes = FALSE,
    colour = "blue", size = 1.8
  ) +

  ## extreme proportions at y = 2 (laa = 0.4) and y = 3 (laa = 0.7)
  # geom_text(
  #   data = ext_summary,
  #   aes(x = param_set_ord,
  #       y = y_anno,
  #       label = label),
  #   inherit.aes = FALSE,
  #   size  = 3.5,
  #   colour  = "red",
  #   fontface = "bold"
  # ) +


  scale_fill_manual(
    values = c("0.1" = "grey60",  "1" = "black"),
    name   = "True value"
  ) +
  ylab(bquote(lambda^a ~ "MLE")) +
  xlab("Parameter Set") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x  = element_blank()
  )
p_laa

ggsave("script/forResults/mle_data/p_laa.pdf",
       plot = p_laa,
       width = 8.5,        # Standard paper width
       height = 3,
       device = cairo_pdf, # Better font handling
       dpi = 300)

# place plots 1 colomn layout, without x-axis labels for top 4 plots ----
combined_plot <- (p_lac +
  p_mu +
  p_K +
  p_gam +
  p_laa) +
  plot_layout(ncol = 1, guides = "collect")

ggsave("script/forResults/mle_data/combined_mle_plots.pdf",
       plot = combined_plot,
       width = 8.5,        # Standard paper width
       height = 15,
       device = cairo_pdf, # Better font handling
       dpi = 300)




