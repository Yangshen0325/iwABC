
# use this to get the ss of observed data and then plot the correlation between them

# Function wrapping up all summary statistics
#' @title Summary statistics

calc_all_stats <- function(sim) {


# # ABC SR ----------------------------------------------------------------


  lastRowId_sim <- nrow(sim[[1]][["stt_all"]])
  num_nonend_sim <- as.numeric(sim[[1]][["stt_all"]][lastRowId_sim, "nI"])
  num_sington_sim <- as.numeric(sim[[1]][["stt_all"]][lastRowId_sim, "nA"])
  num_multi_sim <- as.numeric(sim[[1]][["stt_all"]][lastRowId_sim, "nC"])


# ABC NLTT ----------------------------------------------------------------

  brt_sim <- lapply(sim[-1], "[[", "branching_times")

  end_ltt_sim <- end_ltt(sim, brt_sim)

  nonend_ltt <- end_ltt_sim$nonend_ltt
  singleton_ltt <- end_ltt_sim$singleton_ltt
  multi_ltt <- end_ltt_sim$multi_ltt

  #NLTT-nonend
  if (nonend_ltt[1, 1] == 0) {
    nonend_nltt <- 0
  } else {
  nonend_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = nonend_ltt$nonend_brt,
    species_number = nonend_ltt$n_nonend,
    event_times2 = 0,
    species_number2 = 0,
    distance_method = "abs",
    time_unit = "ago",
    normalize = FALSE
  )
  }

  #NLTT-singleton
  if (singleton_ltt[1, 1] == 0) {
    singleton_nltt <- 0
  } else {
  singleton_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = singleton_ltt$singleton_brt,
    species_number = singleton_ltt$n_singleton,
    event_times2 = 0,
    species_number2 = 0,
    distance_method = "abs",
    time_unit = "ago",
    normalize = FALSE
  )
  }

  #NLTT-multi
  if (multi_ltt[1, 1] == 0) {
    multi_nltt <- 0
  } else {
  multi_nltt <- nLTT::nltt_diff_exact_extinct(
    event_times = multi_ltt$multi_brt,
    species_number = multi_ltt$n_multi,
    event_times2 = 0,
    species_number2 = 0,
    distance_method = "abs",
    time_unit = "ago",
    normalize = FALSE
  )
  }


  # ABC CD ----------------------------------------------------------------
  # first clade
  first_clade_size <- first_clade_size(sim)
  # proportion of the largest clade
  largest_clade_size <- largest_clade_size(sim)
  num_total_sim <- num_sington_sim + num_multi_sim + num_nonend_sim
  prop_largest_clade <- largest_clade_size / num_total_sim
  # rank of the largest clade
  rank_largest_clade <- get_rank_largest_clade(sim)
  # clade evenness
  clade_evenness <- calc_clade_evenness(sim)
  # colonisation time standard deviation
  sim_ct_sd <- colon_time_sd(sim)
  # number of colonisation
  num_colon <- 1000 - sim[[1]][["not_present"]]


# return  -----------------------------------------------------------------


  return(tibble::tibble(
    num_nonend_sim = num_nonend_sim,
    num_sington_sim = num_sington_sim,
    num_multi_sim = num_multi_sim,

    nonend_nltt = nonend_nltt,
    singleton_nltt = singleton_nltt,
    multi_nltt = multi_nltt,

    #sim_cs_sd = sim_cs_sd,
    #largest_clade_size = largest_clade_size,
    first_clade_size = first_clade_size,
    prop_largest_clade = prop_largest_clade,
    rank_largest_clade = rank_largest_clade,
    clade_evenness = clade_evenness,

    sim_ct_sd = sim_ct_sd,
    num_colon = num_colon
  ))
}
