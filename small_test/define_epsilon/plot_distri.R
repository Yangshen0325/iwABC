# Visualize ---------------------------------------------------------------

# read data
t_ss_distri <- readRDS("~/Downloads/phd_yang/pkgs/iwABC/small_test/define_epsilon/t_ss_distri.rds")
# format data
ss_diff_list <- lapply(t_ss_distri, function(x) x$ss_diff_list[[1]])

# set column names
column_names <- c(
  "diff_total_nltt", "diff_singleton_nltt", "diff_multi_nltt", "diff_nonend_nltt",
  "diff_colon_time", "diff_clade_size",
  "diff_total", "diff_sington", "diff_multi", "diff_noend",
  "diff_colon",
  "diff_largest_cs", "diff_first_cs",
  "diff_prop_largest_clade"
)
ss_diff_table <- lapply(ss_diff_list, function(mat) {
  colnames(mat) <- column_names
  return(mat)
})

# combine data across scenarios
combined_data <- do.call(rbind, lapply(seq_along(ss_diff_table), function(i) {
  data <- as.data.frame(ss_diff_table[[i]])
  data$Scenario <- i  # Add a column for the scenario number
  return(data)
}))





# Plotting
library(tidyverse)

# longer data
long_data <- combined_data  |>
  pivot_longer(
    cols = -Scenario,
    names_to = "Statistic",
    values_to = "Difference"
  )

# Define plot functions
plot_box <- function(data, title) {
  ggplot(data, aes(x = as.factor(Scenario), y = Difference, fill = as.factor(Scenario))) +
    geom_boxplot(outlier.alpha = 0.3) +
    facet_wrap(~Statistic, scales = "free", ncol = 1) +
    labs(
      title = title,
      x = "Scenario",
      y = "Difference",
      fill = "Scenario"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}


# group summary statistics for plotting
groups <- list(
  group1 = c("diff_total_nltt", "diff_singleton_nltt", "diff_multi_nltt", "diff_nonend_nltt"),
  group2 = c("diff_colon_time", "diff_clade_size", "diff_colon"),
  group3 = c("diff_total", "diff_songton", "diff_multi", "diff_noend"),
  group4 = c("diff_largest_cs", "diff_first_cs", "diff_prop_largest_clade")

)

# box plot
for (group_name in names(groups)) {

  group <- groups[[group_name]]

  # Filter data for the group
  group_data <- long_data  |>
    filter(Statistic %in% group)

  # Create box plot
  box_plot <- plot_box(group_data, paste("Box Plot for", group_name))
  print(box_plot)

}


# calculate the median and 95th values
summary_stats <- long_data  |>
  group_by(Statistic)  |>
  summarize(
    median = median(Difference),
    Percentile_95 = quantile(Difference, probs = 0.95),
    max_val = max(Difference)
  )

print(summary_stats)

# A tibble: 14 × 4
# Statistic               median Percentile_95 max_val
# <chr>                    <dbl>         <dbl>   <dbl>
#   1 diff_clade_size          1.13          3.63   24.6
# 2 diff_colon               8            24      47
# 3 diff_colon_time          0.299         1.18    2.43
# 4 diff_first_cs            2            10      46
# 5 diff_largest_cs          4            13      45
# 6 diff_multi              14            56      82
# 7 diff_multi_nltt         34.0         133.    265.
# 8 diff_noend               3             9      45
# 9 diff_nonend_nltt         4.53         22.4   118.
# 10 diff_prop_largest_clade  0.124         0.538   0.898
# 11 diff_singleton_nltt     12.7          47.1   152.
# 12 diff_sington             4            16      45
# 13 diff_total              17            59      91
# 14 diff_total_nltt         42.0         145.    293.

# If you want to plot density plot (too many lines, not recommended) --------


plot_density <- function(data, title) {
  ggplot(data, aes(x = Difference, fill = as.factor(Scenario), color = as.factor(Scenario))) +
    geom_density(alpha = 0.3) +
    facet_wrap(~Statistic, scales = "free", ncol = 1) +
    labs(
      title = title,
      x = "Difference",
      y = "Density",
      fill = "Scenario",
      color = "Scenario"
    ) +
    theme_minimal()
}

# box plot
for (group_name in names(groups)) {

  group <- groups[[group_name]]

  # filter data
  group_data <- long_data  |>
    filter(Statistic %in% group)

  density_plot <- plot_density(group_data, paste("Density Plot for", group_name))
  print(density_plot)

}


print(density_plot)










