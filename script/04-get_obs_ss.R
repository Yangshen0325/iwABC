


#### To get the correlation between summary statistics of observed data

rm(list= ls())
library(ggplot2)

# Read observed data
iw_observations <- readRDS("data/iw_observations.rds")

output <- lapply(iw_observations, calc_all_stats)
output_df <- bind_rows(output)

glimpse(output_df)

# # Spearman correlation matrix using ggcorr
# GGally::ggcorr(output_df,
#                method = c("everything", "spearman"),
#                label = TRUE,
#                label_size = 3,
#                hjust = 0.75,
#                layout.exp = 1,
#                low = "skyblue", mid = "white", high = "tomato") +
#   ggtitle("Spearman Correlation of Summary Statistics")

library(corrplot)

# Compute correlation matrix
cor_mat <- cor(output_df, method = "spearman", use = "pairwise.complete.obs")

corrplot::corrplot(cor_mat, method = "color", type = "upper",
                   tl.col = "black", tl.cex = 0.8,
                   col = colorRampPalette(c("skyblue", "white", "tomato"))(200))
