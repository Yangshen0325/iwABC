# Get the standard deviation of clade size of simulation output
clade_size_sd <- function(sim){
  clade_size <- c()
  if (length(sim) == 1){
    clade_size_sd <- 0
  } else {
    for(i in 2:length(sim)){ ##clades
      clade_size[i - 1] <- length(sim[[i]]$branching_times) - 1
    }
    if(length(clade_size) == 1){
      clade_size_sd <- 0
    } else{
      clade_size_sd <- sd(clade_size)
    }
  }
  return(clade_size_sd)
}



# Get the difference between the clade size of two simulations (sd)
calc_clade_size_error <- function(sim_1, sim_2){
  sim1_cs_sd <- clade_size_sd(sim_1)
  sim2_cs_sd <- clade_size_sd(sim_2)
  clade_size_error <- abs(sim1_cs_sd - sim2_cs_sd)
  return(clade_size_error)
}
