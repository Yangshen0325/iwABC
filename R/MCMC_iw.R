

#' Run mcmc DAISIE-IW
#'
#' @return
#' @export
#'



# Read data
# param_space <- read_csv("param_space.csv")
# iw_observations <- readRDS("iw_observation.rds")
#
# # Confirm matching length
# stopifnot(nrow(param_space) == length(iw_observations))
#
# # Parameter indices to estimate
# idparsopt <- c(1, 2, 3, 4, 5)  # can be changed later
#
# obs_sim_pars <- param_space[param_set, ]
# obs_sim <- iw_observations[[param_set]]
#
# # A little perturbation to avoid local optima
# initparsopt <- as.numeric(obs_sim_pars[c(1:5)])
# for(n in 1:5){
#   initparsopt[n]<-exp(log(initparsopt[n]) +
#                         stats::rnorm(1, 0, 0.01))+ 0.000001
# }
#
# MCMC_iw <- function(datalist = obs_sim,
#                     parameters = initparsopt,
#                     iterations = 1000000,
#                     burnin = 100000,
#                     thinning = 400,
#                     sigma = 0.2,
#                     idparsopt = idparsopt) {
#   # Create a list for the chain
#   chain <- array(dim = c(
#     floor(iterations / thinning) + 1,
#     length(parameters) + 2
#   ))
#
#   # pre-compute current loglikelihood
#   log_lik <- calc_log_lik_DAISIE_iw(
#     params = parameters,
#     datalist = datalist
#   ) # one value of loglik
#
#   log_prior <- calc_log_prior_DAISIE_iw(
#     params = parameters,
#     idparsopt = idparsopt
#   )
#
#   cat("\nGenerating Chain\n")
#   cat("0--------25--------50--------75--------100\n")
#   cat("*")
#   utils::flush.console()
#   print_frequency <- 20
#
#   for (i in seq_len(burnin + iterations)) {
#     # propose new values
#     parameters_old <- parameters
#     parameters[idparsopt] <- exp(stats::rnorm(
#       length(idparsopt),
#       log(parameters[idparsopt]), #  log-transform current values
#       sigma # sample normal noise centered at the log of the current values.
#     )) # sigma is the standard deviation of the noise. It controls how far each proposal jumps
#
#     # calculate the Hastings ratio
#     hr <- 0  # Hastings ratio is 0 because proposal distribution is symmetric in log-space
#     new_log_lik <- calc_log_lik_DAISIE_iw(
#       params = parameters,
#       datalist = datalist
#     )
#
#     new_log_prior <- calc_log_prior_DAISIE_iw(
#       params = parameters,
#       idparsopt = idparsopt
#     )
#
#     # accept or reject
#     if (is.finite(new_log_lik) &&
#       is.finite(new_log_prior) &&
#       is.finite(hr) &&
#       new_log_lik + new_log_prior - (log_lik + log_prior) + hr > log(stats::runif(1, 0, 1))) { # in log scale
#       # Accept
#       log_lik <- new_log_lik
#       log_prior <- new_log_prior
#     } else {
#       # Reject → go back to previous parameters
#       parameters <- parameters_old
#     }
#
#     # sample the parameter
#     if (i >= burnin) {
#       if ((i) %% ((iterations - burnin) / print_frequency) == 0) {
#         cat("**")
#         utils::flush.console()
#       }
#       if ((i - burnin) %% thinning == 0) {
#         chain[(i - burnin) / thinning + 1, ] <- c(parameters, log_lik, log_prior)
#       }
#     }
#   }
#   cat("\nFinished MCMC.\n")
#   return(coda::as.mcmc(chain))
# }
