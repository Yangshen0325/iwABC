

rm(list = ls())

library(tidymodels)
tidymodels_prefer()

# read data
ml_df <- readRDS("script/proj4_ML/ml_df.rds")
all_estimates <- readRDS("script/proj4_ML/all_estimates.rds")
true_params <- readRDS("script/proj4_ML/true_params.rds")



# e.g., which method estimated lac best? ----------------------------------


estimate_with_true  <- all_estimates %>%
  left_join(true_params, by = c("param_set", "rep_id"))

estimate_with_true %>%
  group_by(method) %>%
  summarise(RMSE_lac = sqrt(mean((lac_est - lac_true)^2, na.rm = TRUE)),
            .groups = "drop") # remove NA values



# ML pipeline, e.g., only NLTT information? -------------------------------------------------------------
# After splitting data into training and testing sets, we will build a random forest model.
# Use only NLTT information to predict lac_true.
# Can replace NLTT with Species Richness or Clade Distribution

# 01 data split
set.seed(123)
spi_split <- initial_split(ml_df, prop = 0.8)

spi_train <- training(spi_split)
spi_test  <- testing(spi_split)
# > nrow(spi_train)
# [1] 3840
# > nrow(spi_test)
# [1] 960

# 02 recipe
# predictors: nonend_nltt, singleton_nltt, multi_nltt

rf_recipe_lac_nltt <- recipe(
  lac_true ~ nonend_nltt + singleton_nltt + multi_nltt,
  data = spi_train
)

# 03 model specification
rf_model <- rand_forest(
  mtry = 3, # 3 predictors at each split
  trees = 500,
  min_n = 5 # minimum node size
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# 04 workflow
rf_wf_lac_nltt <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(rf_recipe_lac_nltt)

# 05 fit
rf_fit_lac_nltt <- rf_wf_lac_nltt %>%
  fit(data = spi_train)

rf_fit_lac_nltt

# 06 predict
rf_preds <- rf_fit_lac_nltt %>%
  predict(new_data = spi_test) %>%
  bind_cols(spi_test %>% select(lac_true))

rf_preds

# 07 evaluate, yardstick
rf_metrics <- rf_preds %>%
  metrics(truth = lac_true, estimate = .pred)
rf_metrics

rf_metrics %>%
  filter(.metric == "rmse")


# ML pipeline, wrapped a function -----------------------------------------

fit_rf_predict_param <- function(df_train,
                                 df_test,
                                 outcome,
                                 predictors,
                                 trees = 500,
                                 mtry = NULL,
                                 min_n = 5) {

  # If user does not specify mtry, use sqrt rule
  if (is.null(mtry)) {
    mtry <- max(1, floor(sqrt(length(predictors))))
  }

  # Build formula dynamically
  formula_rf <- as.formula(
    paste(outcome, "~", paste(predictors, collapse = " + "))
  )

  # 1. recipe
  rf_recipe <- recipe(formula_rf, data = df_train)

  # 2. model spec
  rf_model <- rand_forest(
    mtry  = mtry,
    trees = trees,
    min_n = min_n
  ) %>%
    set_engine("ranger") %>%
    set_mode("regression")

  # 3. workflow
  rf_wf <- workflow() %>%
    add_model(rf_model) %>%
    add_recipe(rf_recipe)

  # 4. fit
  rf_fit <- rf_wf %>% fit(df_train)

  # 5. predict on test set
  preds <- rf_fit %>%
    predict(new_data = df_test) %>%
    bind_cols(df_test %>% select(all_of(outcome)))

  # 6. compute RMSE
  rmse_res <- rmse(preds, truth = !!sym(outcome), estimate = .pred)

  # return everything
  return(list(
    model = rf_fit,
    predictions = preds,
    rmse = rmse_res
  ))
}

# NLTT predicting lac
nltt_predictors <- c("nonend_nltt", "singleton_nltt", "multi_nltt")

rf_lac_nltt <- fit_rf_predict_param(
  df_train = spi_train,
  df_test  = spi_test,
  outcome = "lac_true",
  predictors = nltt_predictors
)

rf_lac_nltt$rmse

# Species Richness predicting lac
sr_predictors <- c("num_nonend_sim", "num_sington_sim", "num_multi_sim")

rf_lac_sr <- fit_rf_predict_param(
  spi_train, spi_test,
  outcome = "lac_true",
  predictors = sr_predictors
)

rf_lac_sr$rmse

# Clade Distribution predicting lac
cd_predictors <- c("first_clade_size", "prop_largest_clade",
"rank_largest_clade", "clade_evenness")

rf_lac_cd <- fit_rf_predict_param(
  spi_train, spi_test,
  outcome = "lac_true",
  predictors = cd_predictors
)

rf_lac_cd$rmse

# All predictors?

all_predictors <- c("num_nonend_sim","num_sington_sim","num_multi_sim",
                    "nonend_nltt","singleton_nltt","multi_nltt",
                    "first_clade_size","prop_largest_clade",
                    "rank_largest_clade","clade_evenness",
                    "sim_ct_sd","num_colon")

rf_lac_all12 <- fit_rf_predict_param(
  spi_train, spi_test,
  outcome = "lac_true",
  predictors = all_predictors
)

rf_lac_all12$rmse












# Focus on comparison -----------------------------------------------------

# NLTT summary statistics (ss_set = 2)
nltt_predictors <- c("nonend_nltt", "singleton_nltt", "multi_nltt")

# Five true parameters we care about
params <- c("lac_true", "mu_true", "K_true", "gam_true", "laa_true")

# Loop over parameters and fit RF models using NLTT stats
ml_nltt_results <- list()
ml_nltt_rmse <- tibble()

for (par in params) {
  cat("Fitting RF for", par, "using NLTT stats...\n")

  fit_obj <- fit_rf_predict_param(
    df_train   = spi_train,
    df_test    = spi_test,
    outcome    = par,
    predictors = nltt_predictors
  )

  # Save full result (model + predictions + rmse)
  ml_nltt_results[[par]] <- fit_obj

  # Save RMSE in a tidy table
  ml_nltt_rmse <- bind_rows(
    ml_nltt_rmse,
    tibble(
      parameter = par,
      method    = "ML_NLTT",
      rmse      = fit_obj$rmse$.estimate
    )
  )
}

# ml_nltt_results <- list()
ml_nltt_rmse <- tibble()

for (par in params) {
  cat("Fitting RF for", par, "using NLTT stats...\n")

  fit_obj <- fit_rf_predict_param(
    df_train   = spi_train,
    df_test    = spi_test,
    outcome    = par,
    predictors = nltt_predictors
  )

  # Save full result (model + predictions + rmse)
  ml_nltt_results[[par]] <- fit_obj

  # Save RMSE in a tidy table
  ml_nltt_rmse <- bind_rows(
    ml_nltt_rmse,
    tibble(
      parameter = par,
      method    = "ML_NLTT",
      rmse      = fit_obj$rmse$.estimate
    )
  )
}

# Build RMSE comparison table: MLE + ABC + ML

estimates_with_true <- all_estimates %>%
  left_join(true_params, by = c("param_set", "rep_id"))

rmse_abc_mle <- estimates_with_true %>%
  group_by(method) %>%   # method = "MLE", "ABC_SR", "ABC_NLTT", "ABC_CD"
  summarise(
    lac = sqrt(mean((lac_est - lac_true)^2, na.rm = TRUE)),
    mu  = sqrt(mean((mu_est  - mu_true )^2, na.rm = TRUE)),
    K   = sqrt(mean((K_est  - K_true )^2, na.rm = TRUE)),
    gam = sqrt(mean((gam_est - gam_true)^2, na.rm = TRUE)),
    laa = sqrt(mean((laa_est - laa_true)^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols      = c(lac, mu, K, gam, laa),
    names_to  = "parameter",
    values_to = "rmse"
  )

ml_nltt_rmse_clean <- ml_nltt_rmse %>%
  mutate(
    parameter = dplyr::case_when(
      parameter == "lac_true" ~ "lac",
      parameter == "mu_true"  ~ "mu",
      parameter == "K_true"   ~ "K",
      parameter == "gam_true" ~ "gam",
      parameter == "laa_true" ~ "laa",
      TRUE ~ parameter
    )
  )

rmse_all <- bind_rows(
  rmse_abc_mle,
  ml_nltt_rmse_clean
)


# Visualize ML vs ABC vs MLE ----------------------------------------------


ggplot(rmse_all, aes(x = method, y = rmse)) +
  geom_col() +
  facet_wrap(~ parameter, scales = "free_y") +
  coord_flip() +
  labs(
    x = "Method",
    y = "RMSE",
    title = "RMSE of DAISIE-IW parameter estimates by method",
    subtitle = "Small-phylogeny islands, NLTT-based ML added"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey90"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggplot(rmse_all, aes(x = parameter, y = rmse, color = method, group = method)) +
  geom_point(position = position_dodge(width = 0.4)) +
  labs(
    x = "Parameter",
    y = "RMSE",
    title = "Comparison of methods across parameters"
  ) +
  theme_bw(base_size = 12)


