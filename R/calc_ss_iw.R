#' Calculate summary statistic differences between simulated data and a specific
#' replicate of (simulated) observed data.
#'
#' @param sim1 A datalist of observed data with more than one replicate.
#' @param sim2 A datalist of simulated data created by DAISIE IW simulation model.
#' @param ss_set A numeric value indicating which set of summary statistics to use.
#'
#' @export


calc_ss_iw <- function(sim1, sim2, ss_set){

  if (ss_set == 0){ # all
    ss <- calc_error_all(sim_1 = sim1,
                         sim_2 = sim2)
  } else if(ss_set == 1) { # phylogenetic
    ss <- calc_error_phylo(sim_1 = sim1,
                           sim_2 = sim2)
  } else if (ss_set == 2){ # tips
    ss <- calc_error_tips(sim_1 = sim1,
                          sim_2 = sim2)
  } else if (ss_set == 3){ # nltt
    ss <- calc_error_nltt(sim_1 = sim1,
                          sim_2 = sim2)
  }
  ss_diff <- as.numeric(ss)

  return(ss_diff)
}
#### except for ss_set = 0, the other functions are not implemented yet.
