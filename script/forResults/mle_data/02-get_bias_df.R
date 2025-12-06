

## This script calculates the absolute bias between MLE estimates and true parameter values
# abs(est - true) and saves the resulting data frame as `mle_true_bias.rds`.

rm(list = ls())
#### Get the `mle_true_bias` data set ####

# Read mle data frames
res_fornow <-  c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11",
                 "12", "13", "14", "15", "16", "17", "18", "19",  "20",
                 "21", "22", "23", "24", "25", "26","27", "28", "29", "30","31", "32",
                 "33", "34", "35","36", "37", "38", "39", "40", "41", "42",
                 "43", "44", "45", "46", "47", "48")

mle_list <- vector("list", length(res_fornow))

for (i in seq_along(res_fornow)) {
  j <- as.integer(res_fornow[i])
  mle_list[[i]] <- readRDS(paste0("script/forResults/mle_data/mle_df_data/mle_df_", j, ".rds"))

  # Number of successful replicates
  n_found <- nrow(mle_list[[i]])
  # If fewer than 100, pad NA rows
  if(n_found < 100) {
    message("Set", res_fornow[i], ": Found ", n_found, " rows; padding to 100.")
    # Create NA rows matching df structure
    n_missing <- 100 - n_found
    df_na <- mle_list[[i]][rep(1, n_missing), , drop = FALSE]  # duplicate first row
    df_na[,] <- NA                                   # fill with NA
    mle_list[[i]] <- bind_rows(mle_list[[i]], df_na)
  }

}
names(mle_list) <- res_fornow

mle_list_df <- bind_rows(mle_list, .id = "param_set")

mle_list_df <- mle_list_df %>%
  mutate(param_set = as.integer(param_set))

# Read the true values
parameter_space <- read.csv("data/parameter_space.csv")

parameter_space <-parameter_space %>%
  mutate(param_set = row_number())


# Join the true values with the mle results
mle_true_df <- mle_list_df %>%
  left_join(parameter_space, by = "param_set")

mle_true_bias <- mle_true_df %>%
  mutate(
    bias_lac = abs(lac_MLE - lac),
    bias_mu = abs(mu_MLE - mu),
    bias_K   = abs(K_MLE - K),
    bias_gam  = abs(gam_MLE - gam),
    bias_laa  = abs(laa_MLE - laa),
    bias_netdiv = bias_lac - bias_mu
  )
saveRDS(mle_true_bias, "script/forResults/mle_data/mle_true_bias.rds")




