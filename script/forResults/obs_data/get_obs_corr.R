


#### To get the correlation between summary statistics of observed data

rm(list= ls())
library(ggplot2)

# Read observed data
iw_observations <- readRDS("data/iw_observations_rep100_small_k.rds")

output <- lapply(iw_observations, calc_all_stats)
output_df <- bind_rows(output)
saveRDS(output_df, file = "script/obs_ss.rds")
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


output_df <- obs_ss
# Compute correlation matrix
cor_mat <- cor(output_df, method = "spearman", use = "pairwise.complete.obs")

corrplot::corrplot.mixed(cor_mat, method = "color",
                   type = "upper",
                   tl.col = "black", tl.cex = 0.8,
                   col = colorRampPalette(c("skyblue", "white", "tomato"))(200))



# colnames(output_df) <- c(
#   "N_non-end", "N_single-end", "N_multi-end",
#   "NLTT_non-end", "NLTT_single-end", "NLTT_multi-end",
#   "C_first", "P_max", "R_max", "E_clade",
#   "σ_CT", "N_col"
# )

cor_mat <- cor(output_df, method = "spearman", use = "pairwise.complete.obs")

pdf("~/Downloads/phd_yang/ABC_IW/figures/obs_ss_corr.pdf", width = 8, height = 8)

corrplot::corrplot(cor_mat,
                   method = "square",
                   type = "full",
                   diag = TRUE,
                   addCoef.col = "black",
                   number.cex = 0.6,
                   tl.col = "black",
                   tl.cex = 0.8,
                   col = colorRampPalette(c("skyblue", "white", "tomato"))(200))
dev.off()


