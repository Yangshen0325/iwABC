#' prior density function
#'
#' @return Density of the given parameter set
#' @export
prior_dens <- function(pars, idparsopt) {

  if(1 %in% idparsopt){
    dens_lac <- stats::dunif(pars[1], 0, 1)
  } else {
    dens_lac <- 1
  }

  if(2 %in% idparsopt){
    dens_mu <- stats::dunif(pars[2], 0, 0.5)
  } else {
    dens_mu <- 1
  }

  if(3 %in% idparsopt){
    dens_k <- stats::dunif(pars[3], 0, 2000)
  } else {
    dens_k <- 1
  }

  if(4 %in% idparsopt){
    dens_gam <- stats::dunif(pars[4], 0, 0.05)
  } else {
    dens_gam <- 1
  }

  if(5 %in% idparsopt){
    dens_laa <- stats::dunif(pars[5], 0, 1.5)
  } else {
    dens_laa <- 1
  }

  return(dens_lac * dens_mu * dens_k * dens_gam * dens_laa)
}


#'prior function to generate parameters
#'
#' @return a vector of parameters
#' @export
prior_gen <- function(pars,idparsopt){

  if(1 %in% idparsopt){
    lac <- stats::runif(1, 0, 1)
  } else {
    lac <- pars[1]
  }

  if(2 %in% idparsopt){
    mu <- stats::runif(1, 0, 0.5)
  } else {
    mu <- pars[2]
  }

  if(3 %in% idparsopt){
    k <- stats::runif(1, 0, 2000)
  } else {
    k <- pars[3]
  }

  if(4 %in% idparsopt){
    gam <- stats::runif(1, 0, 0.05)
  } else {
    gam <- pars[4]
  }

  if(5 %in% idparsopt){
    laa <- stats::runif(1, 0, 1.5)
  } else {
    laa <- pars[5]
  }
  return(as.numeric(c(lac, mu, k, gam, laa)))
}


