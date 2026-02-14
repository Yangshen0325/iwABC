
# Note: Don't use it! Becasue it contain info for each param_set, each iteration
# and each single rep id. It's too large data set! We extract what we need.
# Pls refer to 02-spi_get_true_bias.R


# Get the posterior data from small-phylogeny islands outputs.
# Extract param_set_X_ss_0$ABC and format them (20 iterations and use Thijs's fast
# DAISIE sim pkg)
# Not all data gained. Pre test
s
rm(list = ls())
library(tidyverse)
library(purrr)
##### Sm##### Sm##### Small-Phylogeny Islands ABC results

# read ABC posterior results and make it tidy tibble
# param_set rep iteration particle lac mu K gam laa

file_paths <- list.files("script/forResults/ABC_data/spi_ABC",
                         pattern = "^param_set_\\d+_ss_0\\.rds$", full.names = TRUE)

file_paths <- list.files("~/Downloads/temp/ABC_spi_ss1",
                         pattern = "^param_set_\\d+_ss_1\\.rds$", full.names = TRUE)

file_paths <- list.files("~/Downloads/temp/ABC_spi_ss2",
                         pattern = "^param_set_\\d+_ss_2\\.rds$", full.names = TRUE)
# Function to read and extract posterior samples
extract_abc_samples <- function(file) {
  file_num   <- as.integer(str_extract(basename(file), "\\d+"))
  param_set  <- floor((file_num - 1) / 100) + 1
  rep        <- ((file_num - 1) %% 100) + 1

  rds_obj    <- readRDS(file)
  abc_list   <- rds_obj$ABC

  map2_dfr(
    abc_list,
    .y = seq_along(abc_list),
    .f = function(mat, iter) {
      colnames(mat) <- c("lac", "mu", "K", "gam", "laa")
      as_tibble(mat) %>%
        mutate(
          param_set = param_set,
          rep       = rep,
          iteration = iter,
          particle  = row_number()
        )
    }
  )
}

# Apply function to all files and bind
posterior_all <- map_dfr(file_paths, extract_abc_samples)
# Inspect
# glimpse(posterior_all)

# Read true values
parameter_space <- read.csv("data/parameter_space.csv")

true_values <- parameter_space %>%
  mutate(param_set = row_number())

posterior_joined <- posterior_all %>%
  left_join(true_values, by = "param_set", suffix = c("_est", "_true")) %>%
              mutate(netdiv_est  = lac_est - mu_est,
                     netdiv_true = lac_true - mu_true)

saveRDS(posterior_joined, "script/forResults/ABC_data/spi_ABC_posterior_joined.rds")
# glimpse(posterior_joined)

saveRDS(posterior_joined, "script/forResults/ABC_data/spi_ABC_posterior_joined_ss1.rds")


saveRDS(posterior_joined, "script/forResults/ABC_data/spi_ABC_posterior_joined_ss2.rds")


