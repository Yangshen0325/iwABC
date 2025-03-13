


# Plot sigma_i v.s. i
sigma_0 <- 0.05
i <- seq(1, 100, 1)
sigma_i <-  sigma_0 * exp(-0.2 * (i -1))
plot(sigma_i ~ i, type = "l", xlab = "i", ylab = "sigma_i", main = "sigma_i v.s. i", col = "blue")
