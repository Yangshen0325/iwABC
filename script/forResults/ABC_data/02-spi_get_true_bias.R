
# Don't run, output saved.

# First read the ABC output from cluster (copied to local 'temp' folder), process, and
# then combine it with the true values.
# The final output has the same structure as the MLE output, and bias is the
# absolute difference.

rm(list = ls())

library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(tibble)


# for SPI -----------------------------------------------------------------

# ss0 data only has 10 reps and 10 iterations.

# Note: if making MLE vs ABC (with different ss) scatter plot, read
# "~/Downloads/temp/new_ABC_spi_ss1/iter10_10reps" etc. instead. DONE! Output saved. :)

file_paths <- list.files("~/Downloads/temp/new_ABC_spi_ss0/iter10_10reps",
                         pattern = "^param_set_\\d+_ss_0\\.rds$", full.names = TRUE)

file_paths <- list.files("~/Downloads/temp/new_ABC_spi_ss1",
                         pattern = "^param_set_\\d+_ss_1\\.rds$", full.names = TRUE)

file_paths <- list.files("~/Downloads/temp/new_ABC_spi_ss2",
                         pattern = "^param_set_\\d+_ss_2\\.rds$", full.names = TRUE)

file_paths <- list.files("~/Downloads/temp/new_ABC_spi_ss3",
                         pattern = "^param_set_\\d+_ss_3\\.rds$", full.names = TRUE)

file_paths <- list.files("~/Downloads/temp/new_ABC_spi_ss4",
                         pattern = "^param_set_\\d+_ss_4\\.rds$", full.names = TRUE)

# 1. Loop over existing files and store medians

med_list <- vector("list", length(file_paths))

for (i in seq_along(file_paths)) {

  the_file <- file_paths[i]

  # e.g. "param_set_7_ss_0.rds" -> 7
  file_num <- as.integer(str_extract(basename(the_file), "\\d+"))

  # 1–100 -> param_set 1, 101–200 -> param_set 2, ... up to 4800 -> 48
  param_set <- floor((file_num - 1) / 100) + 1

  # --- read RDS and get ABC list ---
  obj_list <- readRDS(the_file)
  abc_list <- obj_list$ABC

  # last iteration (20th): 500 × 5
  last_iter <- abc_list[[15]]
  last_iter <- as.matrix(last_iter)

  # parameter names
  colnames(last_iter) <- c("lac_ABC", "mu_ABC", "K_ABC", "gam_ABC", "laa_ABC")

  # --- median value over the 500 particles ---
  median_vec <- apply(last_iter, 2, median)

  # store as one row
  med_list[[i]] <- data.frame(
    file_num = file_num,
    param_set = param_set,
    t(median_vec),
    row.names = NULL
  )
}

# bind all existing files
med_raw <- do.call(rbind, med_list)


# 2. Create full 1–4800 template and fill missing as NA

all_param_sets  <- 1:48
reps_per_set    <- 100

template <- expand.grid(
  param_set = all_param_sets,
  rep_in_set = 1:reps_per_set
)

template$file_num <- (template$param_set - 1) * reps_per_set + template$rep_in_set

# merge, keeping all 4800 rows; missing files become NA
med_full <- merge(
  template[, c("file_num", "param_set")],
  med_raw,
  by = c("file_num", "param_set"),
  all.x = TRUE,
  sort = TRUE
)

# delete the first column
med_full$file_num <- NULL

# Read the true values
parameter_space <- read.csv("data/parameter_space.csv")

parameter_space <-parameter_space %>%
  mutate(param_set = row_number())

# Join the true values with the mle results
abc_true_df <- med_full %>%
  left_join(parameter_space, by = "param_set")

abc_true_bias <- abc_true_df %>%
  mutate(
    bias_lac = abs(lac_ABC - lac),
    bias_mu = abs(mu_ABC - mu),
    bias_K   = abs(K_ABC - K),
    bias_gam  = abs(gam_ABC - gam),
    bias_laa  = abs(laa_ABC - laa),
    bias_netdiv = abs(lac_ABC - mu_ABC)
  )
saveRDS(abc_true_bias, "script/forResults/ABC_data/abc_true_bias_ss1.rds")

saveRDS(abc_true_bias, "script/forResults/ABC_data/abc_true_bias_ss2.rds")

saveRDS(abc_true_bias, "script/forResults/ABC_data/abc_true_bias_ss3.rds")

saveRDS(abc_true_bias, "script/forResults/ABC_data/abc_true_bias_ss4.rds")



# for LPI -----------------------------------------------------------------
rm(list = ls())

file_paths <- list.files("script/forResults/ABC_data/lpi_ABC",
                         pattern = "^param_set_\\d+_ss_0\\.rds$", full.names = TRUE)

# 1. Loop over existing files and store medians

med_list <- vector("list", length(file_paths))

for (i in seq_along(file_paths)) {

  the_file <- file_paths[i]

  # e.g. "param_set_7_ss_0.rds" -> 7
  file_num <- as.integer(str_extract(basename(the_file), "\\d+"))

  # 1–10 -> param_set 1, 11–20 -> param_set 2, ... up to 320 -> 32
  param_set <- floor((file_num - 1) / 10) + 1

  # --- read RDS and get ABC list ---
  obj_list <- readRDS(the_file)
  abc_list <- obj_list$ABC

  # last iteration (10th): 500 × 5
  last_iter <- abc_list[[15]]
  last_iter <- as.matrix(last_iter)

  # parameter names
  colnames(last_iter) <- c("lac_ABC", "mu_ABC", "K_ABC", "gam_ABC", "laa_ABC")

  # --- median value over the 500 particles ---
  median_vec <- apply(last_iter, 2, median)

  # store as one row
  med_list[[i]] <- data.frame(
    file_num = file_num,
    param_set = param_set,
    t(median_vec),
    row.names = NULL
  )
}

# bind all existing files
med_raw <- do.call(rbind, med_list)


# 2. Create full 1–320 template and fill missing as NA

all_param_sets  <- 1:32
reps_per_set    <- 10

template <- expand.grid(
  param_set = all_param_sets,
  rep_in_set = 1:reps_per_set
)

template$file_num <- (template$param_set - 1) * reps_per_set + template$rep_in_set

# merge, keeping all 320 rows; missing files become NA
med_full <- merge(
  template[, c("file_num", "param_set")],
  med_raw,
  by = c("file_num", "param_set"),
  all.x = TRUE,
  sort = TRUE
)

# delete the first column
med_full$file_num <- NULL

# Read the true values
param_space <- utils::read.csv("data/parameter_space_rep100_large_k.csv")
param_space <- param_space %>%
  filter(reps == 1)

parameter_space <- param_space %>%
  select(-reps) %>%
  mutate(param_set = row_number())

# Join the true values with the mle results
abc_true_df <- med_full %>%
  left_join(parameter_space, by = "param_set")

abc_true_bias <- abc_true_df %>%
  mutate(
    bias_lac = abs(lac_ABC - lac),
    bias_mu = abs(mu_ABC - mu),
    bias_K   = abs(K_ABC - K),
    bias_gam  = abs(gam_ABC - gam),
    bias_laa  = abs(laa_ABC - laa),
    bias_netdiv = bias_lac - bias_mu
  )

saveRDS(abc_true_bias, "script/forResults/ABC_data/LPI_abc_true_bias_ss0.rds")

