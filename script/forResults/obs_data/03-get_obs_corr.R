


#### To get the correlation between summary statistics of observed dat, SPI ####

rm(list= ls())
library(ggplot2)
library(corrplot)



# SPI ---------------------------------------------------------------------


# Read observed data
SPI_all_stats <- readRDS("script/forResults/obs_data/SPI_all_stats.rds")


# # Spearman correlation matrix using ggcorr
# GGally::ggcorr(output_df,
#                method = c("everything", "spearman"),
#                label = TRUE,
#                label_size = 3,
#                hjust = 0.75,
#                layout.exp = 1,
#                low = "skyblue", mid = "white", high = "tomato") +
#   ggtitle("Spearman Correlation of Summary Statistics")

output_df <- SPI_all_stats %>%
  select(!param_set)
# Compute correlation matrix
cor_mat <- cor(output_df, method = "spearman", use = "pairwise.complete.obs")


# colnames(output_df) <- c(
#   "N_non-end", "N_single-end", "N_multi-end",
#   "NLTT_non-end", "NLTT_single-end", "NLTT_multi-end",
#   "C_first", "P_max", "R_max", "E_clade",
#   "σ_CT", "N_col"
# )

pdf("~/Downloads/phd_yang/ABC_IW/figures/SPI_correlation_corrplot.pdf", width = 8, height = 8)
 corrplot::corrplot(cor_mat,
                   method = "square",
                   type = "full",
                   diag = TRUE,
                   addCoef.col = "black",
                   number.cex = 0.6,
                   tl.col = "black",
                   tl.cex = 0.8,
                   col = colorRampPalette(c("#4575b4", "white", "#d73027"))(200))
 dev.off()


# For LPI -----------------------------------------------------------------
 rm(list= ls())
 # Read observed data
 LPI_all_stats <- readRDS("script/forResults/obs_data/LPI_all_stats.rds")

 output_df <- LPI_all_stats %>%
   select(!param_set)
 # Compute correlation matrix
 cor_mat <- cor(output_df, method = "spearman", use = "pairwise.complete.obs")

 pdf("~/Downloads/phd_yang/ABC_IW/figures/LPI_correlation_corrplot.pdf", width = 8, height = 8)
 corrplot::corrplot(cor_mat,
                    method = "square",
                    type = "full",
                    diag = TRUE,
                    addCoef.col = "black",
                    number.cex = 0.6,
                    tl.col = "black",
                    tl.cex = 0.8,
                    col = colorRampPalette(c("#4575b4", "white", "#d73027"))(200))
 dev.off()


