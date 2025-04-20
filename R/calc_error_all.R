#' Calculate summary statistic distances between two simulated trees when using all statistics
#' Diversity difference: `num_nonend`, `num_sington`, `num_multi`
#' NLTT difference: `nonend_nltt`, `singleton_nltt`, `multi_nltt`
#' Clade size difference: `sd_clade_size`, `largest_cs_diff`, `first_cs_diff`, `prop_largest_clade_diff`
#' Colonisation difference: `sd_colon_time`, `num_colon`
#'
#' @return A vector with all error metrics:
#' @export
#'
calc_error_all <- function(sim_1,
                           sim_2,
                           replicates = 1,
                           distance_method = "abs") {
  # Diversity difference ----------------------------------------------------


  # difference in nonendemic species richness
  lastRowId_sim_1 <- nrow(sim_1[[1]][["stt_all"]])
  lastRowId_sim_2 <- nrow(sim_2[[1]][["stt_all"]])


  num_nonend_sim_1 <- as.numeric(sim_1[[1]][["stt_all"]][lastRowId_sim_1, "nI"])
  num_nonend_sim_2 <- as.numeric(sim_2[[1]][["stt_all"]][lastRowId_sim_2, "nI"])
  num_nonend <-
    abs(num_nonend_sim_1 - num_nonend_sim_2)

  # difference of anagenesis species richness
  num_sington_sim_1 <- as.numeric(sim_1[[1]][["stt_all"]][lastRowId_sim_1, "nA"])
  num_sington_sim_2 <- as.numeric(sim_2[[1]][["stt_all"]][lastRowId_sim_2, "nA"])
  num_sington <-
    abs(num_sington_sim_1 - num_sington_sim_2)

  # difference of cladogenesis species richness
  num_multi_sim_1 <- as.numeric(sim_1[[1]][["stt_all"]][lastRowId_sim_1, "nC"])
  num_multi_sim_2 <- as.numeric(sim_2[[1]][["stt_all"]][lastRowId_sim_2, "nC"])
  num_multi <-
    abs(num_multi_sim_1 - num_multi_sim_2)

  # delete this to save time, more accuracy with this, maybe more rejected particles though.
  # num_total_sim_1 <- num_sington_sim_1 + num_multi_sim_1 + num_nonend_sim_1
  # num_total_sim_2 <- num_sington_sim_2 + num_multi_sim_2 + num_nonend_sim_2
  # num_total <- abs(num_total_sim_1 - num_total_sim_2)

  # NLTT difference -------------------------------------------------------

  # branching times of two simulation outputs
  brt1 <- lapply(sim_1[-1], "[[", "branching_times")
  brt2 <- lapply(sim_2[-1], "[[", "branching_times")

  ltt_1 <- full_ltt(sim_1, brt1) # a table of two cols, one with all branching times in descending order,
  ltt_2 <- full_ltt(sim_2, brt2) # one with the number of species 0,1,2,3,4...

  # Delete this to save time
  # total number species nltt error
  # total_nltt <- nLTT::nltt_diff_exact_extinct(
  #   event_times = ltt_1$brt,
  #   species_number = ltt_1$n_spec,
  #   event_times2 = ltt_2$brt,
  #   species_number2 = ltt_2$n_spec,
  #   distance_method = distance_method,
  #   time_unit = "ago",
  #   normalize = FALSE
  # )

  ## nonendemic_nltt and singleton-endemic-nltt and multi-endemic-nltt
  end_ltt_1 <- end_ltt(sim_1, brt1)
  end_ltt_2 <- end_ltt(sim_2, brt2)

  nonend_ltt_1 <- end_ltt_1$nonend_ltt
  nonend_ltt_2 <- end_ltt_2$nonend_ltt

  # total number of nonendemic species nltt error
  if (nonend_ltt_1[1, 1] == 0 && nonend_ltt_2[1, 1] == 0) {
    nonend_nltt <- 0
  } else {
    nonend_nltt <- nLTT::nltt_diff_exact_extinct(
      event_times = nonend_ltt_1$nonend_brt,
      species_number = nonend_ltt_1$n_nonend,
      event_times2 = nonend_ltt_2$nonend_brt,
      species_number2 = nonend_ltt_2$n_nonend,
      distance_method = distance_method,
      time_unit = "ago",
      normalize = FALSE
    )
  }

  singleton_ltt_1 <- end_ltt_1$singleton_ltt
  singleton_ltt_2 <- end_ltt_2$singleton_ltt
  # total number of singleton species nltt error
  if (singleton_ltt_1[1, 1] == 0 && singleton_ltt_2[1, 1] == 0) {
    singleton_nltt <- 0
  } else {
    singleton_nltt <- nLTT::nltt_diff_exact_extinct(
      event_times = singleton_ltt_1$singleton_brt,
      species_number = singleton_ltt_1$n_singleton,
      event_times2 = singleton_ltt_2$singleton_brt,
      species_number2 = singleton_ltt_2$n_singleton,
      distance_method = distance_method,
      time_unit = "ago",
      normalize = FALSE
    )
  }

  multi_ltt_1 <- end_ltt_1$multi_ltt
  multi_ltt_2 <- end_ltt_2$multi_ltt
  # total number of cladogenetic species nltt error
  if (multi_ltt_1[1, 1] == 0 && multi_ltt_2[1, 1] == 0) {
    multi_nltt <- 0
  } else {
    multi_nltt <- nLTT::nltt_diff_exact_extinct(
      event_times = multi_ltt_1$multi_brt,
      species_number = multi_ltt_1$n_multi,
      event_times2 = multi_ltt_2$multi_brt,
      species_number2 = multi_ltt_2$n_multi,
      distance_method = distance_method,
      time_unit = "ago",
      normalize = FALSE
    )
  }


  # Clade size difference ---------------------------------------------------------

  # standard deviation of clade size error
  sd_clade_size <- calc_clade_size_error(sim_1, sim_2)

  # the largest clade size difference
  largest_clade_size_1 <- largest_clade_size(sim_1)
  largest_clade_size_2 <- largest_clade_size(sim_2)
  largest_cs_diff <- abs(largest_clade_size_1 - largest_clade_size_2)

  # the first clade size difference
  first_clade_size_1 <- first_clade_size(sim_1)
  first_clade_size_2 <- first_clade_size(sim_2)
  first_cs_diff <- abs(first_clade_size_1 - first_clade_size_2)

  # proportion of the largest clades
  num_total_sim_1 <- num_sington_sim_1 + num_multi_sim_1 + num_nonend_sim_1
  num_total_sim_2 <- num_sington_sim_2 + num_multi_sim_2 + num_nonend_sim_2
  prop_largest_clade_1 <- largest_clade_size_1 / num_total_sim_1
  prop_largest_clade_2 <- largest_clade_size_2 / num_total_sim_2
  prop_largest_clade_diff <- abs(prop_largest_clade_1 - prop_largest_clade_2)



  # Colonisation difference  -------------------

  # standard deviation of colonization time
  sd_colon_time <- calc_colon_time_error(sim_1, sim_2)

  # number of colonization events
  num_colon_1 <- 1000 - sim_1[[1]][["not_present"]]
  num_colon_2 <- 1000 - sim_2[[1]][["not_present"]]
  num_colon <- abs(num_colon_1 - num_colon_2)


  # All errors --------------------------------------------------------------
  return(
    c(num_nonend,
      num_sington,
      num_multi,

      nonend_nltt,
      singleton_nltt,
      multi_nltt,

      sd_clade_size,
      largest_cs_diff,
      first_cs_diff,
      prop_largest_clade_diff,

      sd_colon_time,
      num_colon

    )
  )
}





