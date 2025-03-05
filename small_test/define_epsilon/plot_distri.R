# Visualize ---------------------------------------------------------------
rm(list=ls())
# read data
t_ss_distri <- readRDS("small_test/define_epsilon/t_ss_distri.rds")
# format data
groups <- split(t_ss_distri, rep(1:32, each = 10))

col_names <- c("diff_nonend", "diff_singleton", "diff_multi",
               "diff_nonend_nltt", "diff_singleton_nltt", "diff_multi_nltt",
               "diff_sd_cs", "diff_largest_cs", "diff_first_cs", "diff_prop_largest_clade",
               "diff_sd_colontime", "diff_colon")

groups <- lapply(groups, function(group) {
  lapply(group, function(mat) {
    # If mat is a matrix, convert it to a data frame for easier handling.
    df <- as.data.frame(mat)
    colnames(df) <- col_names
    return(df)
  })
})
# For each group, add a "Group" column and then combine all replicates.
combined_list <- lapply(seq_along(groups), function(g) {
  # groups[[g]] is a list of 10 data frames for group g.
  lapply(groups[[g]], function(df) {
    df$Group <- g  # Add group identifier
    return(df)
  })
})

# Unlist the nested lists (flatten to one list of data frames)
flat_list <- unlist(combined_list, recursive = FALSE)

# Combine all data frames into one data frame.
combined_data <- do.call(rbind, flat_list)


# Plotting
library(tidyverse)

# longer data
long_data <- combined_data  |>
  pivot_longer(
    cols = -Group,
    names_to = "Statistic",
    values_to = "Difference"
  )

# Define plot functions
plot_box <- function(data, title) {
  ggplot(data, aes(x = as.factor(Group), y = Difference, fill = as.factor(Group))) +
    geom_boxplot(outlier.alpha = 0.3) +
    facet_wrap(~Statistic, scales = "free", ncol = 1) +
    labs(
      title = title,
      x = "Group",
      y = "Difference",
      fill = "Group"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}


# group summary statistics for plotting
types <- list(
  type1 = c("diff_nonend", "diff_singleton", "diff_multi"),
  type2 = c("diff_nonend_nltt", "diff_singleton_nltt", "diff_multi_nltt"),
  type3 = c("diff_sd_cs", "diff_largest_cs", "diff_first_cs", "diff_prop_largest_clade"),
  type4 = c("diff_sd_colontime", "diff_colon")
)

# box plot
for (type_name in names(types)) {

  the_type <- types[[type_name]]

  # Filter data for the group
  type_data <- long_data  |>
    filter(Statistic %in% type)

  # Create box plot
  box_plot <- plot_box(type_data, paste("Box Plot for", type_name))
  print(box_plot)

}


# calculate the median and 95th values
summary_stats <- long_data  |>
  group_by(Statistic)  |>
  summarize(
    median = median(Difference),
    Percentile_95 = stats::quantile(Difference, probs = 0.95),
    max_val = max(Difference)
  )

print(summary_stats)

# A tibble: 12 × 4
# Statistic                median Percentile_95 max_val
# <chr>                     <dbl>         <dbl>   <dbl>
#   1 diff_colon              888           901     907
# 2 diff_first_cs             2            14      57
# 3 diff_largest_cs           3            15      57
# 4 diff_multi                9            43      87
# 5 diff_multi_nltt          20.7         108.    280.
# 6 diff_nonend               2            11      49
# 7 diff_nonend_nltt          2.18         19.2   147.
# 8 diff_prop_largest_clade   0.467         0.909   0.982
# 9 diff_sd_colontime         0.869         1.75    3.48
# 10 diff_sd_cs                1.06          5.51   36.0
# 11 diff_singleton            5            18      54
# 12 diff_singleton_nltt      15.1          54.4   187.

# If you want to plot density plot (too many lines, not recommended) --------


plot_density <- function(data, title) {
  ggplot(data, aes(x = Difference, fill = as.factor(Group), color = as.factor(Group))) +
    geom_density(alpha = 0.3) +
    facet_wrap(~Statistic, scales = "free", ncol = 1) +
    labs(
      title = title,
      x = "Difference",
      y = "Density",
      fill = "Group",
      color = "Group"
    ) +
    theme_minimal()
}

# density plot
for (type_name in names(types)) {

  the_type <- types[[type_name]]

  # filter data
  type_data <- long_data  |>
    filter(Statistic %in% type)

  density_plot <- plot_density(type_data, paste("Density Plot for", type_name))
  print(density_plot)

}


print(density_plot)










