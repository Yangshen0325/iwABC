# Run on cluster
library(iwABC)

# Maximum-likelihood estimation

# Read data
iw_observations <- readRDS("script/iw_observations.rds")

# Apply get_MLE to each sublist in iw_observations
MLE_list <- lapply(iw_observations, get_MLE)

# Convert the list of results into a data frame
MLE_df <- do.call(rbind, lapply(results_list, as.data.frame))


save(MLE_df,
     file = "script/MLE_df.rds")
