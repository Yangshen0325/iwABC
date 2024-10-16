# Generate parameter space (using Shu's parameter setting + initialise K_0)

rm(list = ls())

# Define the parameters
lac <- c(0.4, 0.7)
mu <- c(0, 0.3)
K <- c(20, 200)
gam <- c(0.003, 0.009)
laa <- c(0.1, 1.0)

# Define replicates
rep <- 10


# Create all combinations
param_space <- expand.grid(lac = lac,
                           mu = mu,
                           K = K,
                           gam = gam,
                           laa = laa
                           )

# Replicate each row of the parameter space 10 times
rep_param_space <- param_space[rep(seq_len(nrow(param_space)), each = rep), ]

# Add a column for the replicate number (1 to 10)
rep_param_space$rep <- rep(1:rep, times = nrow(param_space))

# Save
write.csv(rep_param_space, "data/parameter_space.csv", row.names = FALSE)
