


# Get true parameters, MLE results and all ABC results

library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())

# Read data
abc_true_bias_ss1 <- readRDS("script/forResults/abc_data/abc_true_bias_ss1.rds")

abc_true_bias_ss2 <- readRDS("script/forResults/abc_data/abc_true_bias_ss2.rds")

abc_true_bias_ss3 <- readRDS("script/forResults/abc_data/abc_true_bias_ss3.rds")

mle_true_bias <- readRDS("script/forResults/mle_data/mle_true_bias.rds")

SPI_all_stats <- readRDS("script/forResults/obs_data/SPI_all_stats.rds")

# Combine all estimates with all summary stats
# 01 deal with "Set1" in SPI_all_stats and add one coloumn for rep_id
SPI_all_stats <- SPI_all_stats %>%
  mutate(param_set = parse_number(param_set)) %>%
  group_by(param_set) %>%
  mutate(rep_id = row_number()) %>%
  ungroup()
# 02 add rep_id for ABC and MLE
add_rep_id <- function(df) {
  df %>%
    group_by(param_set) %>%
    mutate(rep_id = row_number()) %>%
    ungroup()
}

abc_ss1_with_id <- add_rep_id(abc_true_bias_ss1)
abc_ss2_with_id <- add_rep_id(abc_true_bias_ss2)
abc_ss3_with_id <- add_rep_id(abc_true_bias_ss3)
mle_with_id <- add_rep_id(mle_true_bias)

# 03 get true parameters
true_params <- mle_with_id %>%
  select(
    param_set, rep_id,
    lac_true = lac,
    mu_true  = mu,
    K_true   = K,
    gam_true = gam,
    laa_true = laa
  )

# 04 get MLE estimates, and ABC estimates for all 3 summary stats sets
# MLE
mle_estimates <- mle_with_id %>%
  transmute(
    param_set,
    rep_id,
    method = "MLE",
    lac_est = lac_MLE,
    mu_est  = mu_MLE,
    K_est   = K_MLE,
    gam_est = gam_MLE,
    laa_est = laa_MLE
  )
# ABC
abc_sr_estimates <- abc_ss1_with_id %>%
  transmute(
    param_set,
    rep_id,
    method = "ABC_SR",
    lac_est = lac_ABC,
    mu_est  = mu_ABC,
    K_est   = K_ABC,
    gam_est = gam_ABC,
    laa_est = laa_ABC
  )

abc_nltt_estimates <- abc_ss2_with_id %>%
  transmute(
    param_set,
    rep_id,
    method = "ABC_NLTT",
    lac_est = lac_ABC,
    mu_est  = mu_ABC,
    K_est   = K_ABC,
    gam_est = gam_ABC,
    laa_est = laa_ABC
  )

abc_cd_estimates <- abc_ss3_with_id %>%
  transmute(
    param_set,
    rep_id,
    method = "ABC_CD",
    lac_est = lac_ABC,
    mu_est  = mu_ABC,
    K_est   = K_ABC,
    gam_est = gam_ABC,
    laa_est = laa_ABC
  )
# last step: bind all together
estimates_long <- bind_rows(
  mle_estimates,
  abc_sr_estimates,
  abc_nltt_estimates,
  abc_cd_estimates
)

# Prep data for ML
ml_df <- SPI_all_stats %>%
  # only  param_set, rep_id, 12 stats
  left_join(true_params, by = c("param_set", "rep_id")) %>%
  relocate(param_set, rep_id)



# save all data -----------------------------------------------------------

saveRDS(ml_df, "script/proj4_ML/ml_df.rds")
saveRDS(estimates_long, "script/proj4_ML/all_estimates.rds")
saveRDS(true_params, "script/proj4_ML/true_params.rds")

