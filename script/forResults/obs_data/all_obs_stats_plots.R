
rm(list = ls())
library(tidyverse)

# Calculate the info of "pesudo empirical data", DON"T RUN (have it)---------------------------
# The data has been collected in the file "small_test/obs_summary/output_df.rds"
# This .rds file is result of large K for only ABC simulation outputs (so 3200 in total,
# and K is 100 and 1000)

# read the data
output_df <- readRDS("small_test/obs_summary/output_df.rds")

# read parameter space
parameter_space <- read_csv("data/parameter_space_rep100_large_k.csv",
                            show_col_types = FALSE)

## combine them together
# first rename `output_df` column `combo` to `reps`, and change it
# 32 times from 1 to 100

output_df <- output_df %>%
  rename(reps = combo) %>%
  mutate(reps = rep(1:100, times = 32))


# then combine them together
all_obs_df <- cbind(parameter_space, output_df[, -1]) %>%
  relocate("reps") %>%
  mutate(
    lac_mu = factor(
      paste0("lac=", lac, ", mu=", mu)
    ),
    gam_laa = factor(
      paste0("gam=", gam, ", laa=", laa)
    )
  )


saveRDS(all_obs_df, "script/forResults/all_obs_df.rds")




# Make tables of the overview of "pesudo empirical data" ----------------

# read data
all_obs_df <- readRDS("script/forResults/obs_data/all_obs_df.rds")

all_obs_stats <- all_obs_df %>%
  group_by(lac, mu, K, gam, laa) %>%
  summarise(
    across(
      .cols = -c(reps, lac_mu, gam_laa),
      .fns = list(
        mean = ~ mean(.x),
        sd = ~ sd(.x),
        median = ~ median(.x),
        IQR = ~ IQR(.x)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

saveRDS(all_obs_stats, "script/forResults/all_obs_stats.rds")

# Make plots of the overview of "pesudo empirical data" -------------------


# read data
all_obs_df <- readRDS("script/forResults/obs_data/all_obs_df.rds")

# Plot: 4 × 4 grid, with one panel per (gam_laa) × (lac_mu).
#
#    · Within each panel, draw two boxplots:
#        • x = K (factor), y = species
#        • fill = K (so that K=100 vs K=1000 are colored differently)
#    · facet_grid(rows = gam_laa, cols = lac_mu, switch = "both")

all_obs_df$lac_mu <- factor(all_obs_df$lac_mu,
                            levels = c("lac=0.4, mu=0",
                                       "lac=0.7, mu=0",
                                       "lac=0.4, mu=0.3",
                                       "lac=0.7, mu=0.3"))

all_obs_df$gam_laa <- factor(all_obs_df$gam_laa,
                            levels = c("gam=0.001, laa=0.1",
                                       "gam=0.002, laa=0.1",
                                       "gam=0.001, laa=1",
                                       "gam=0.002, laa=1"))

all_obs_df$K <- factor(all_obs_df$K, levels = c(100, 1000))

# Check
# > levels(all_obs_df$lac_mu)
# [1] "lac=0.4, mu=0"   "lac=0.7, mu=0"   "lac=0.4, mu=0.3" "lac=0.7, mu=0.3"
# > levels(all_obs_df$gam_laa)
# [1] "gam=0.001, laa=0.1" "gam=0.002, laa=0.1" "gam=0.001, laa=1.0" "gam=0.002, laa=1.0"

label_map_lac_mu <- c(
  "lac=0.4, mu=0"   = "lambda^c==0.4~','~mu==0",
  "lac=0.7, mu=0"   = "lambda^c==0.7~','~mu==0",
  "lac=0.4, mu=0.3" = "lambda^c==0.4~','~mu==0.3",
  "lac=0.7, mu=0.3" = "lambda^c==0.7~','~mu==0.3"
)

label_map_gam_laa <- c(
  "gam=0.001, laa=0.1" = "gamma==0.001~','~lambda^a==0.1",
  "gam=0.002, laa=0.1" = "gamma==0.002~','~lambda^a==0.1",
  "gam=0.001, laa=1" = "gamma==0.001~','~lambda^a==1.0",
  "gam=0.002, laa=1" = "gamma==0.002~','~lambda^a==1.0"
)

#### Plot the number of colonisation per K (carrying capacity) ####
ggplot(all_obs_df, aes(x = K, y = num_colon, fill = K)) +
  geom_boxplot() +
  scale_fill_manual(values = c("100" = "#4C72B0", "1000" = "#FF6347")) +
  facet_grid(rows = vars(gam_laa),
             cols = vars(lac_mu),
             labeller = labeller(
               gam_laa = as_labeller(label_map_gam_laa,  default = label_parsed),
               lac_mu  = as_labeller(label_map_lac_mu,   default = label_parsed)
             )) +
  labs(x = "K (carrying capacity)",
       y = "Number of Colonisation") +
  theme_bw() +
  theme(
    panel.spacing         = unit(0.5, "lines"),
    axis.text.x           = element_text(angle = 0, vjust = 0.5),
    axis.text.y           = element_text(size = 8),
    plot.title            = element_text(hjust = 0.5, face = "bold"),
    legend.position       = "bottom"
  )
ggsave("script/forResults/obs_data/num_colon_per_K.png", width = 8, height = 6, dpi = 300)


#### Plot the clade evenness per K   ####
ggplot(all_obs_df, aes(x = K, y = clade_evenness, fill = K)) +
  geom_boxplot() +
  scale_fill_manual(values = c("100" = "#4C72B0", "1000" = "#FF6347")) +
  facet_grid(rows = vars(gam_laa),
             cols = vars(lac_mu),
             labeller = labeller(
               gam_laa = as_labeller(label_map_gam_laa,  default = label_parsed),
               lac_mu  = as_labeller(label_map_lac_mu,   default = label_parsed)
             )) +
  labs(x = "K (carrying capacity)",
       y = "Clade Evenness") +
  theme_bw() +
  theme(
    panel.spacing         = unit(0.5, "lines"),
    axis.text.x           = element_text(angle = 0, vjust = 0.5),
    axis.text.y           = element_text(size = 8),
    plot.title            = element_text(hjust = 0.5, face = "bold"),
    legend.position       = "bottom"
  )
ggsave("script/forResults/obs_data/clade_evenness_per_K.png", width = 8, height = 6, dpi = 300)



#### Plot the first clade size per K   ####
ggplot(all_obs_df, aes(x = K, y = first_clade_size, fill = K)) +
  geom_boxplot() +
  scale_fill_manual(values = c("100" = "#4C72B0", "1000" = "#FF6347")) +
  facet_grid(rows = vars(gam_laa),
             cols = vars(lac_mu),
             labeller = labeller(
               gam_laa = as_labeller(label_map_gam_laa,  default = label_parsed),
               lac_mu  = as_labeller(label_map_lac_mu,   default = label_parsed)
             )) +
  labs(x = "K (carrying capacity)",
       y = "First Clade Size") +
  theme_bw() +
  theme(
    panel.spacing         = unit(0.5, "lines"),
    axis.text.x           = element_text(angle = 0, vjust = 0.5),
    axis.text.y           = element_text(size = 8),
    plot.title            = element_text(hjust = 0.5, face = "bold"),
    legend.position       = "bottom"
  )
ggsave("script/forResults/obs_data/first_clade_size_per_K.png", width = 8, height = 6, dpi = 300)


#### Plot the total number of species, endemic species per K   ####
sprichness_tb <- all_obs_df %>%
  mutate(total_sp = num_nonend_sim + num_sington_sim + num_multi_sim,
         endemic_sp = num_sington_sim + num_multi_sim)
# total species
ggplot(sprichness_tb, aes(x = K, y = total_sp, fill = K)) +
  geom_boxplot() +
  scale_fill_manual(values = c("100" = "#4C72B0", "1000" = "#FF6347")) +
  facet_grid(rows = vars(gam_laa),
             cols = vars(lac_mu),
             labeller = labeller(
               gam_laa = as_labeller(label_map_gam_laa,  default = label_parsed),
               lac_mu  = as_labeller(label_map_lac_mu,   default = label_parsed)
             )) +
  labs(x = "K (carrying capacity)",
       y = "Total Species Richness") +
  theme_bw() +
  theme(
    panel.spacing         = unit(0.5, "lines"),
    axis.text.x           = element_text(angle = 0, vjust = 0.5),
    axis.text.y           = element_text(size = 8),
    plot.title            = element_text(hjust = 0.5, face = "bold"),
    legend.position       = "bottom"
  )
ggsave("script/forResults/obs_data/total_species_per_K.png", width = 8, height = 6, dpi = 300)

# endemic species
ggplot(sprichness_tb, aes(x = K, y = endemic_sp, fill = K)) +
  geom_boxplot() +
  scale_fill_manual(values = c("100" = "#4C72B0", "1000" = "#FF6347")) +
  facet_grid(rows = vars(gam_laa),
             cols = vars(lac_mu),
             labeller = labeller(
               gam_laa = as_labeller(label_map_gam_laa,  default = label_parsed),
               lac_mu  = as_labeller(label_map_lac_mu,   default = label_parsed)
             )) +
  labs(x = "K (carrying capacity)",
       y = "Endemic Species Richness") +
  theme_bw() +
  theme(
    panel.spacing         = unit(0.5, "lines"),
    axis.text.x           = element_text(angle = 0, vjust = 0.5),
    axis.text.y           = element_text(size = 8),
    plot.title            = element_text(hjust = 0.5, face = "bold"),
    legend.position       = "bottom"
  )
ggsave("script/forResults/obs_data/endemic_species_per_K.png", width = 8, height = 6, dpi = 300)
