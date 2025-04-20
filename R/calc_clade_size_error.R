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
      clade_size_sd <- stats::sd(clade_size)
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




# Get the size of largest clade
largest_clade_size <- function(sim){
  clade_size <- c()
  if (length(sim) == 1) {
    largest_clade_size <- 0
  } else {
    for(i in 2:length(sim)){ ##clades
      clade_size[i - 1] <- length(sim[[i]]$branching_times) - 1
    }
    largest_clade_size <- max(clade_size)
  }

  return(largest_clade_size)
}



# Get the fist clade size
first_clade_size <- function(sim){
  clade_size <- c()
  if (length(sim) == 1) {
    first_clade_size <- 0
  } else {
    first_clade_size <- length(sim[[2]]$branching_times) - 1
    }

  return(first_clade_size)
}



# Get the rank of the largest clade based on colonization time
get_rank_largest_clade <- function(sim) {

  if (length(sim) == 1){
    rank_largest <- 0
  } else {

    clade_sublists <- sim[2:length(sim)]  # exclude sim[[1]]
    # Get clade sizes and colonization times
    clade_sizes <- sapply(clade_sublists, function(clade) {
      length(clade$branching_times) - 1})
    colonization_times <- sapply(clade_sublists, function(clade) {
      clade$branching_times[2]})

     # Find index of largest clade
    index_largest_clade <- which.max(clade_sizes)
    # Rank clades by colonization time (bigger = older = ealier)
    colonization_rank <- order(colonization_times, decreasing = TRUE)
    # Find the rank of the largest clade
    rank_largest <- match(index_largest_clade, colonization_rank)
  }
}


