
#library(vegan)  # for diversity metrics
# calculate the clade evenness

calc_clade_evenness <- function(sim) {
  if (length(sim) == 1) {
    J <- 0 # no clades
  } else {
    clade_sublists <- sim[2:length(sim)] # exclude sim[[1]]
    clade_sizes <- sapply(clade_sublists, function(clade) {
      length(clade$branching_times) - 1
    })

    S <- length(clade_sizes) # number of clades

    if (S <= 1) {
      return(0)
    }

    # Shannon evenness
    H <- diversity(clade_sizes, index = "shannon")
    J <- H / log(S)

    return(J) # value between 0 (uneven) and 1 (perfect evenness)
  }
}
