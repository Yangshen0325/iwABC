# Generate parameter space (using Shu's parameter setting + initialise K_0)

rm(list = ls())

## Define the parameters
lac <- c(0.4, 0.7)
mu <- c(0, 0.3)
K <- c(20, 50, 100)
gam <- c(0.001, 0.002)
laa <- c(0.1, 1.0)

## Define replicates
reps <- 100

## Create all combinations
param_space <- expand.grid(lac = lac,
mu = mu,
K = K,
gam = gam,
laa = laa
)

## Replicate each row of the parameter space 10 times
rep_param_space <- param_space[rep(seq_len(nrow(param_space)), each = reps), ]

## Add a column for the replicate number (1 to 10)
rep_param_space$reps <- rep(1:reps, times = nrow(param_space))

## Save
write.csv(rep_param_space, "data/parameter_space_rep100_small_k.csv", row.names = FALSE)


## Make it a function
# generate_param_space <- function(lac, mu, K, gam, laa, rep, path) {
#
#   param_space <- expand.grid(lac = lac,
#                              mu = mu,
#                              K = K,
#                              gam = gam,
#                              laa = laa
#                               )
#
#   rep_param_space <- param_space[rep(seq_len(nrow(param_space)), each = rep), ]
#   rep_param_space$rep <- rep(1:rep, times = nrow(param_space))
#
#   write.csv(rep_param_space, paste0(path, "/parameter_space.csv"), row.names = FALSE)
# }



