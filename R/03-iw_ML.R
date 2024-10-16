# Maximum-likelihood estimation

# Read data
iw_observations <- readRDS("data/iw_observations.rds")

# Apply MLE
# DAISIE_ML_IW <- function(
#     datalist,
#     initparsopt,
#     idparsopt,
#     parsfix,
#     idparsfix,
#     res = 100,
#     ddmodel = 11,
#     cond = 0,
#     tol = c(1E-4, 1E-5, 1E-7),
#     maxiter = 1000 * round((1.25) ^ length(idparsopt)),
#     methode = "ode45",
#     optimmethod = "subplex",
#     verbose = 0,
#     tolint = c(1E-16, 1E-14),
#     jitter = 0,
#     num_cycles = 1)

get_MLE <- function(the_sim) {

  # Record seed for each estimation
  seed_mle <-as.integer(Sys.time()) %% 1000000L * sample(1:10,1)
  set.seed(seed_mle)
  message("seed_mle: ", seed_mle)

  # Apply ML_IW function to the simulation
  MLE_pars <- DAISIE::DAISIE_ML_IW(

    datalist = the_sim,
    initparsopt = c(0.55, 0.15, 110, 0.006, 0.55),
    idparsopt = c(1, 2, 3, 4, 5),
    parsfix = NULL,
    idparsfix = NULL,
    cond = 1
  )

  # Extract relevant outputs
  lac_MLE<-  MLE_pars$lambda_c
  mu_MLE <- MLE_pars$mu
  K_MLE <- MLE_pars$K
  gam_MLE <- MLE_pars$gamma
  laa_MLE <- MLE_pars$lambda_a
  max_ll<-  MLE_pars$loglik

  # Return results as a list
  return(list(
    lac_MLE = lac_MLE,
    mu_MLE = mu_MLE,
    K_MLE = K_MLE,
    gam_MLE = gam_MLE,
    laa_MLE = laa_MLE,
    max_ll = max_ll
  ))

}

# Apply get_MLE to each sublist in iw_observations
results_list <- lapply(iw_observations, get_MLE)

# Convert the list of results into a data frame
results_df <- do.call(rbind, lapply(results_list, as.data.frame))



