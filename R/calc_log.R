

#' Calculates the log prior density
#'
#' @return a numeric represents the log prior density
#' @export

calc_log_prior_DAISIE_iw <- function(params, idparsopt) {
  log_prior <- log(prior_dens(params, idparsopt))
  return(log_prior)
}

#' Calculates the loglikelihood of the DAISIE model with island-wide
#' diversity-dependence given data and a set of model parameters
#'
#' @return a numeric represents the loglikelihood
#' @export
calc_log_lik_DAISIE_iw <- function(params, datalist) {

  log_lik <- DAISIE::DAISIE_loglik_IW(
    pars1 = as.numeric(params[1:5]),
    pars2 = c(100, 11, 0, 1), # ???
    datalist,
    methode = "lsodes"
  )
  return(log_lik)
}

#
