# Get the standard deviation of clonization time of simulation output
colon_time_sd <- function(sim){
  colon_time <- c()
  if (length(sim) == 1){
    colon_time_sd <- 0
  } else {
    for(i in 2:length(sim)){ ##clades
      colon_time[i - 1] <- sim[[i]]$branching_times[2]
    }
    if(length(colon_time) == 1){
      colon_time_sd <- 0
    } else{
      colon_time_sd <- sd(colon_time)
    }
  }
  return(colon_time_sd)
}

# Get the difference between the colonazation time of two simulations (sd)
calc_colon_time_error <- function(sim_1, sim_2){
  sim1_ct_sd <- colon_time_sd(sim_1)
  sim2_ct_sd <- colon_time_sd(sim_2)
  colon_time_error <- abs(sim1_ct_sd - sim2_ct_sd)
  return(colon_time_error)
}
