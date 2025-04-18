#' Calculate weight of each particle
#'
#' @param weights A vector of weights
#' @param particles A list of parameter combinations
#' @param current A vector of current parameter combination to determine the
#'                weight.
#' @param sigma Standard deviation of the pertubation.
#' @param prior_density_function Function to calculate the prior probability.
#'
#' @return A numeric as the estimated weight
#' @author Shu Xie
#' @export


calc_weight <- function(weights, particles,
                        current, sigma, prior_density_function,idparsopt) {
  vals <- c()
  for (i in seq_along(particles)) {
    vals[i] <- weights[i]
    diff <- log(current[idparsopt]) - log(particles[[i]][idparsopt])
    prob <- stats::dnorm(diff, mean = 0, sd = sigma)
    vals[i] <- vals[i] * prod(prob)
  }

  # current_opt <- current[idparsopt]
  numerator <- prior_density_function(current, idparsopt)

  return(numerator / sum(vals))
}
