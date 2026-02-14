
# Copy some code from "script/forResults/obs_data/SPI_observeDatatest.R"

# Present the observed data. Small-phylogeny islands

rm(list = ls())
library(tidyverse)
library(patchwork)
library(purrr)



# Four stats across sets, plain boxes -------------------------------------


SPI_all_stats <- readRDS("script/forResults/obs_data/SPI_all_stats.rds")
# calculate the total number of species
spi_obs <- SPI_all_stats %>%
  mutate(
    total_species = num_nonend_sim + num_sington_sim + num_multi_sim,
    Scenario      = factor(param_set, levels = paste0("Set", 1:48))
  )

# Plot SPI statistics: total species, num colonisations, clade evenness, prop largest clade
spi_obs_long <- spi_obs %>%
  select(
    Scenario,
    total_species,
    num_colon,
    first_clade_size,
    prop_largest_clade,
    rank_largest_clade,
    clade_evenness
  ) %>%
  pivot_longer(
    cols      = -Scenario,
    names_to  = "statistic",
    values_to = "value"
  ) %>%
  mutate(
    statistic = factor(
      statistic,
      levels = c("total_species", "num_colon", "rank_largest_clade",
                 "first_clade_size", "prop_largest_clade", "clade_evenness"),
      labels = c(
        "(a) Species richness",
        "(b) Number of clades",
        "(c) Rank of largest clade",
        "(d) Size of first clade",
        "(e) Proportion of largest clade",
        "(f) Clade evenness"
      )
    )
  )

p_spi_obs <- ggplot(
  spi_obs_long,
  aes(x = Scenario, y = value, fill = statistic)
) +
  geom_boxplot(
    outlier.size = 0.4,
    linewidth    = 0.4,
    alpha        = 0.85,
    na.rm        = TRUE
  ) +
  facet_wrap(
    ~ statistic,
    scales = "free_y",
    ncol   = 3
  ) +
  scale_fill_brewer(
    palette = "Set2",
    guide   = "none"   # colour helps visually but legend not needed
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),

    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),

    strip.background = element_rect(
      fill   = "grey92",
      colour = "grey50",
      linewidth = 0.5
    ),
    strip.text = element_text(face = "bold"),

    panel.spacing = unit(1.1, "lines")
  ) +
  labs(
    x = "Parameter Set",
    y = NULL
  )

p_spi_obs

ggsave(
  "script/forResults/obs_data/SPI_observed_stats_boxes.pdf",
  plot   = p_spi_obs,
  width    = 8,   # adjust if needed
  height   = 4,    # adjust if needed
  units    = "in",
  device   = cairo_pdf,   # ensures proper embedding of fonts
  dpi    = 300
)


# Aggregate sets, compare between params ----------------------------------
rm(list = ls())

SPI_all_stats <- readRDS("script/forResults/obs_data/SPI_all_stats.rds")

spi_obs <- SPI_all_stats %>%
  mutate(
    total_species = num_nonend_sim + num_sington_sim + num_multi_sim
  )

parameter_space <- read_csv("data/parameter_space.csv")
parameter_space <- parameter_space %>%
  mutate(param_set = paste0("Set", 1:48))

join_param_stats <- spi_obs %>%
  left_join(parameter_space, by = "param_set")

stats_col <- c("total_species", "num_colon", "prop_largest_clade", "clade_evenness")

join_param_stats_long <- join_param_stats %>%

  select(param_set, lac, mu, K, gam, laa, all_of(stats_col)) %>%
  pivot_longer(
    cols      = all_of(stats_col),
    names_to  = "statistic",
    values_to = "value"
  ) %>%
  mutate(
    statistic = factor(
      statistic,
      levels = stats_col,
      labels = c("Total number of species",
                 "Number of colonisations",
                 "Proportion of largest clade size",
                 "Clade evenness")
    )
  )


make_global_param_plot <- function(data_long, focal = "lac") {
  # Allowed parameters
  all_params <- c("lac", "mu", "gam", "laa", "K")
  stopifnot(focal %in% all_params)

  # Order of x-axis: increasing values
  focal_vals <- sort(unique(pull(data_long, all_of(focal))))
  focal_fac  <- factor(data_long[[focal]], levels = focal_vals)

  # Pretty x-axis label (optional)
  x_lab <- switch(
    focal,
    "lac" = expression(lambda^c),
    "laa" = expression(lambda^a),
    "gam" = expression(gamma),
    "mu"  = expression(mu),
    "K"   = "K",
    focal
  )

  ggplot(
    data_long,
    aes(
      x    = focal_fac,
      y    = value,
      fill = focal_fac
    )
  ) +
    geom_boxplot(outlier.size = 0.3, alpha = 0.8, na.rm = TRUE) +
    facet_wrap(~ statistic, scales = "free_y", ncol = 2) +
    theme_bw(base_size = 12) +
    theme(
      strip.background = element_rect(colour = "grey70"),
      legend.position  = "none"
    ) +
    labs(
      x = x_lab,
      y = NULL
    )
}

p_spi_lac <- make_global_param_plot(join_param_stats_long, focal = "lac")
p_spi_mu  <- make_global_param_plot(join_param_stats_long, focal = "mu")
p_spi_gam <- make_global_param_plot(join_param_stats_long, focal = "gam")
p_spi_laa <- make_global_param_plot(join_param_stats_long, focal = "laa")
p_spi_K   <- make_global_param_plot(join_param_stats_long, focal = "K")

p_spi_lac + p_spi_mu + p_spi_gam + p_spi_laa + p_spi_K +
  plot_layout(ncol = 2)


## Show sets, but grouped by focal values

library(rlang)

make_paramset_grouped_plot <- function(data_long, focal = "lac") {
  focal_sym <- sym(focal) # give out the focal without quetos. Otherwise, it's
  # not working when using `distinct` and `arrange`.

  # 1. Compute ordering of param_set by focal value (then by name)
  order_tbl <- data_long %>%
    distinct(param_set, !!focal_sym) %>%    # one row per set
    arrange(!!focal_sym, param_set) %>%     # first sort by focal param
    mutate(
      param_set_ord = factor(param_set, levels = param_set),
      focal_value   = !!focal_sym
    )

  # 2. Attach ordered factor back to full long data
  plot_df <- data_long %>%
    left_join(order_tbl %>% select(param_set, param_set_ord, focal_value),
              by = "param_set")

  # 3. Pretty label for x and legend
  x_lab <- paste0("Parameter sets ordered by ", focal)
  fill_lab <- switch(
    focal,
    "lac" = expression(lambda^c),
    "laa" = expression(lambda^a),
    "gam" = expression(gamma),
    "mu"  = expression(mu),
    "K"   = "K",
    focal
  )

  ggplot(plot_df,
         aes(x = param_set_ord,
             y = value,
             fill = factor(focal_value))) +
    geom_boxplot(outlier.size = 0.3, alpha = 0.8, na.rm = TRUE) +
    facet_wrap(~ statistic, scales = "free_y", ncol = 2) +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x  = element_blank(),       # keep axis but hide labels
      axis.ticks.x = element_blank(),
      strip.background = element_rect(colour = "grey70"),
      legend.position  = "bottom"
    ) +
    labs(
      x    = x_lab,
      y    = NULL,
      fill = fill_lab
    )
}

# P1: varying lac
p_P1_lac <- make_paramset_grouped_plot(spi_long, focal = "lac")
# P2: varying mu
p_P2_mu <- make_paramset_grouped_plot(join_param_stats_long, focal = "mu")
# P3: varying gam
p_P3_gam <- make_paramset_grouped_plot(join_param_stats_long, focal = "gam")
# P4: varying laa
p_P4_laa <- make_paramset_grouped_plot(join_param_stats_long, focal = "laa")
# P5: varying K
p_P5_K <- make_paramset_grouped_plot(join_param_stats_long, focal = "K")

























# LPI ---------------------------------------------------------------------
rm(list = ls())

LPI_all_stats <- readRDS("script/forResults/obs_data/LPI_all_stats.rds")
# calculate the total number of species
lpi_obs <- LPI_all_stats %>%
  mutate(
    total_species = num_nonend_sim + num_sington_sim + num_multi_sim,
    Scenario      = factor(param_set, levels = c(1:32))
  )

# Plot LPI statistics: total species, num colonisations, clade evenness, prop largest clade

lpi_obs_long <- lpi_obs %>%
  select(
    Scenario,
    total_species,
    num_colon,
    first_clade_size,
    prop_largest_clade,
    rank_largest_clade,
    clade_evenness
  ) %>%
  pivot_longer(
    cols      = -Scenario,
    names_to  = "statistic",
    values_to = "value"
  ) %>%
  mutate(
    statistic = factor(
      statistic,
      levels = c("total_species", "num_colon", "rank_largest_clade",
                 "first_clade_size", "prop_largest_clade", "clade_evenness"),
      labels = c(
        "(a) Species richness",
        "(b) Number of clades",
        "(c) Rank of largest clade",
        "(d) Size of first clade",
        "(e) Proportion of largest clade",
        "(f) Clade evenness"
      )
    )
  )

p_lpi_obs <- ggplot(
  lpi_obs_long,
  aes(x = Scenario, y = value, fill = statistic)
) +
  geom_boxplot(
    outlier.size = 0.4,
    linewidth    = 0.4,
    alpha        = 0.85,
    na.rm        = TRUE
  ) +
  facet_wrap(
    ~ statistic,
    scales = "free_y",
    ncol   = 3
  ) +
  scale_fill_brewer(
    palette = "Set2",
    guide   = "none"   # colour helps visually but legend not needed
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),

    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),

    strip.background = element_rect(
      fill   = "grey92",
      colour = "grey50",
      linewidth = 0.5
    ),
    strip.text = element_text(face = "bold"),

    panel.spacing = unit(1.1, "lines")
  ) +
  labs(
    x = "Parameter Set",
    y = NULL
  )

p_lpi_obs

ggsave(
  "script/forResults/obs_data/LPI_observed_stats_boxes.pdf",
  plot   = p_lpi_obs,
  width    = 8,   # adjust if needed
  height   = 4,    # adjust if needed
  units    = "in",
  device   = cairo_pdf,   # ensures proper embedding of fonts
  dpi    = 300
)

