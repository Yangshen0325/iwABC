#### Generate plots for illustrating the processes of applying ABC

library(ggplot2)



# 01 Probability density plot of uniform parameters --------------------------

# Define the uniform(0, 2) density
x_vals <- seq(-0.5, 2.5, by = 0.01)
y_vals <- ifelse(x_vals >= 0 & x_vals <= 2, 1 / (2 - 0), 0) # 1/（b-a）
df <- data.frame(x = x_vals, density = y_vals)

p <- ggplot(df, aes(x = x, y = density)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = data.frame(x = c(0, 2), y = c(0.5, 0.5)),
             aes(x = x, y = y), size = 2) +
  #annotate("segment", x = 0, xend = 0, y = 0, yend = 0.5, linetype = "dashed") +
  #annotate("segment", x = 2, xend = 2, y = 0, yend = 0.5, linetype = "dashed") +
  labs(title = "Prior distribution", x = "", y = "Density") +
  coord_cartesian(xlim = c(-0.1, 2.1), ylim = c(0, 0.7)) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid = element_blank()
  )
ggsave("~/Downloads/phd_yang/ABC_IW/figures/01_prior_distribution.png", p, width = 6, height = 4, dpi = 300)


# A different density plot ------------------------------------------------


# Generate values from 0 to 2
x_vals <- seq(0, 2, length.out = 500)

# Rescale Beta(2, 5) to the interval [0, 2]
# Transformation: if X ~ Beta(a, b) on [0, 1], then Y = 2 * X ~ Beta(a, b) on [0, 2]
a <- 2
b <- 5
y_vals <- dbeta(x_vals / 2, shape1 = a, shape2 = b) / 2  # Adjusted for stretching

df <- data.frame(x = x_vals, density = y_vals)

# Plot
p2 <- ggplot(df, aes(x = x, y = density)) +
  geom_line(linewidth = 1.2) +
  labs(x = "", y = "Density") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    axis.text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 18, face = "bold")
  )
ggsave("~/Downloads/phd_yang/ABC_IW/figures/02_prior_distribution.png", p2, width = 6, height = 4, dpi = 300)



# third one ---------------------------------------------------------------

# Define x range
x_vals <- seq(0, 2, length.out = 500)

# Custom bell-shaped hump function
y_vals <- exp(-10 * (x_vals - 1)^2)

df <- data.frame(x = x_vals, y = y_vals)

# Plot
p3 <- ggplot(df, aes(x = x, y = y)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Posterior distribution",
       x = "", y = "Density") +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    panel.grid = element_blank(),
    axis.line = element_line(linewidth = 1.2, colour = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    axis.text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 18, face = "bold")
  )

ggsave("~/Downloads/phd_yang/ABC_IW/figures/03_posterior_distribution.png", p3, width = 6, height = 4, dpi = 300)
