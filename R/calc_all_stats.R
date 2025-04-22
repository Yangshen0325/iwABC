
# use this to get the ss of observed data and then plot the correlation between them

# Function wrapping up all summary statistics
#' @title Summary statistics

calc_all_stats <- function(sim) {

  lastRowId_sim <- nrow(sim[[1]][["stt_all"]])
  num_nonend_sim <- as.numeric(sim[[1]][["stt_all"]][lastRowId_sim, "nI"])
  num_sington_sim <- as.numeric(sim[[1]][["stt_all"]][lastRowId_sim, "nA"])
  num_multi_sim <- as.numeric(sim[[1]][["stt_all"]][lastRowId_sim, "nC"])

  #brt <- lapply(sim[-1], "[[", "branching_times")
  #ltt <- full_ltt(sim, brt)
  #end_ltt <- end_ltt(sim, brt)

  #nonend_ltt <- end_ltt$nonend_ltt
  #singleton_ltt <- end_ltt$singleton_ltt
  #multi_ltt <- end_ltt$multi_ltt

  #sim_cs_sd <- clade_size_sd(sim)

  largest_clade_size <- largest_clade_size(sim)

  first_clade_size <- first_clade_size(sim)

  num_total_sim <- num_sington_sim + num_multi_sim + num_nonend_sim
  prop_largest_clade <- largest_clade_size / num_total_sim

  rank_largest_clade <- get_rank_largest_clade(sim)

  sim_ct_sd <- colon_time_sd(sim)

  num_colon <- 1000 - sim[[1]][["not_present"]]

  clade_evenness <- calc_clade_evenness(sim)

  return(tibble::tibble(
    num_nonend_sim = num_nonend_sim,
    num_sington_sim = num_sington_sim,
    num_multi_sim = num_multi_sim,
    #sim_cs_sd = sim_cs_sd,
    #largest_clade_size = largest_clade_size,
    first_clade_size = first_clade_size,
    prop_largest_clade = prop_largest_clade,
    rank_largest_clade = rank_largest_clade,
    sim_ct_sd = sim_ct_sd,
    num_colon = num_colon,
    clade_evenness = clade_evenness
  ))
}
