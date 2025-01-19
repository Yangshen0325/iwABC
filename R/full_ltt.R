# Get the full stt
full_ltt <- function (sim, brt) {

  recolon <-lapply(sim,"[[", "all_colonisations")
  recolon <- recolon[!unlist(lapply(recolon, is.null))]

  recolon_brt <- list()
  if(length(recolon) > 0){
    for (i in 1: length(recolon)){
      recolon_brt <- append(recolon_brt,lapply(recolon[[i]],"[[", "event_times"))
    }
  }
  brt_full <- append(brt, recolon_brt)
  brt <- c(unique(sort(unlist(brt_full), decreasing = TRUE)), 0)
  n_spec <- c(seq(0, length(brt) - 2), length(brt) - 2)
  full_ltt <- data.frame(brt, n_spec)
  return(full_ltt)
}
