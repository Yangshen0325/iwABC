


# Read me:
# (1) and (2) are functions applying in (3).
# Output in (3) is saved and not run here.
# (4) is plotting code.
# tidymodels framework: https://www.tidymodels.org/start/resampling/

rm(list = ls())

library(tidymodels)
library(tidyverse)

# (1) Function to apply RF to one `outcome` (lac/mu/K/gam/laa)  --------------------------------------------------------------

fit_rf_predict_param <- function(df_train,
                                 df_test,
                                 outcome,
                                 predictors,
                                 trees = 500,
                                 mtry = NULL,
                                 min_n = 5) {

  # not specify mtry? use sqrt rule (standard practice)
  if (is.null(mtry)) {
    mtry <- max(1, floor(sqrt(length(predictors))))
  }

  # formula
  formula_rf <- as.formula(
    paste(outcome, "~", paste(predictors, collapse = " + "))
  )

  # 1. recipe
  rf_recipe <- recipe(formula_rf, data = df_train)

  # 2. model specification
  rf_model <- rand_forest(
    mtry  = mtry,
    trees = trees,
    min_n = min_n
  ) %>%
    set_engine("ranger") %>% # Ranger is a fast implementation of random forests
    set_mode("regression")

  # 3. workflow
  rf_wf <- workflow() %>%
    add_model(rf_model) %>%
    add_recipe(rf_recipe)

  # 4. fit
  rf_fit <- rf_wf %>% fit(df_train) # Here is the fitting process

  # 5. predict on test set
  preds <- rf_fit %>%
    predict(new_data = df_test) %>%
    bind_cols(df_test %>% select(all_of(outcome)))

  # 6. compute RMSE (root mean squared error / standard deviation of residuals)
  rmse_res <- rmse(preds, truth = !!sym(outcome), estimate = .pred)

  return(list(
    model = rf_fit,
    predictions = preds,
    rmse = rmse_res
  ))
}


# (2) Function to apply RF to ALL `param`(lac,mu,K,gam,laa) --------------------------------------------------------------

fit_rf_for_all_params <- function(df_train,
                                  df_test,
                                  params,
                                  predictors,
                                  trees = 500,
                                  mtry = NULL,
                                  min_n = 5) {

  all_results <- list()
  rmse_tbl <- tibble()
  preds_all <- tibble()

  for (par in params) {
    cat("Fitting RF for", par, "...\n") # print out for tracking progress

    res <- fit_rf_predict_param(
      df_train   = df_train,
      df_test    = df_test,
      outcome    = par,
      predictors = predictors,
      trees      = trees,
      mtry       = mtry,
      min_n      = min_n
    )
    # This function returns:
    # return(list(
    #   model = rf_fit,
    #   predictions = preds,
    #   rmse = rmse_res
    # ))

    all_results[[par]] <- res

    rmse_tbl <- bind_rows(
      rmse_tbl,
      tibble(
        parameter = par,
        method    = "ML_RF",
        rmse      = res$rmse$.estimate
      )
    )

    ## Standardize predictions for plotting:
    # rename the true column to "truth" and add parameter label
    preds_std <- res$predictions %>%
      rename(truth = !!par) %>%
      mutate(parameter = par)

    preds_all <- bind_rows(preds_all, preds_std)
  }

  # Clean up parameter names for plotting
  preds_all <- preds_all %>%
    mutate(
      parameter_clean = case_when(
        parameter == "lac_true" ~ "lac",
        parameter == "mu_true"  ~ "mu",
        parameter == "K_true"   ~ "K",
        parameter == "gam_true" ~ "gam",
        parameter == "laa_true" ~ "laa",
        TRUE ~ parameter
      )
    )

  rmse_tbl <- rmse_tbl %>%
    mutate(
      parameter_clean = case_when(
        parameter == "lac_true" ~ "lac",
        parameter == "mu_true"  ~ "mu",
        parameter == "K_true"   ~ "K",
        parameter == "gam_true" ~ "gam",
        parameter == "laa_true" ~ "laa",
        TRUE ~ parameter
      )
    )

  return(list(
    models      = all_results,   # list of fitted workflows
    rmse_table  = rmse_tbl,      # tibble of RMSE by parameter
    predictions = preds_all      # combined preds for plotting
  ))
}





# (3) Apply functions above. Don't run, data saved ---------------------------------------------------------------------

# read data
ml_df <- readRDS("script/proj4_ML/ml_df.rds")
all_estimates <- readRDS("script/proj4_ML/all_estimates.rds")
true_params <- readRDS("script/proj4_ML/true_params.rds")

# define predictors and parameters
nltt_predictors <- c("nonend_nltt", "singleton_nltt", "multi_nltt")
params <- c("lac_true", "mu_true", "K_true", "gam_true", "laa_true")

# data split
set.seed(123)
spi_split <- initial_split(ml_df, prop = 0.8)

spi_train <- training(spi_split)
spi_test  <- testing(spi_split)
# > nrow(spi_train)
# [1] 3840
# > nrow(spi_test)
# [1] 960

rf_all_nltt <- fit_rf_for_all_params(
  df_train   = spi_train,
  df_test    = spi_test,
  params     = params,
  predictors = nltt_predictors
)
saveRDS(rf_all_nltt, "script/proj4_ML/rf_all_nltt.rds")

# (4) Plotting code -------------------------
# read data
rf_all_nltt <- readRDS("script/proj4_ML/rf_all_nltt.rds")

plot_rf_truth_vs_pred <- function(preds_all,
                                  title = "RF predictions vs true values") {
  ggplot(preds_all, aes(x = truth, y = .pred)) +
    geom_point(alpha = 0.3) +
    geom_abline(slope = 1, intercept = 0) +
    facet_wrap(~ parameter_clean, scales = "free") +
    labs(
      x = "True value",
      y = "Predicted value",
      title = title
    ) +
    theme_bw(base_size = 12)
}

p <- plot_rf_truth_vs_pred(rf_all_nltt$predictions,
                      title = "RF (NLTT stats) – prediction vs true for all parameters")



